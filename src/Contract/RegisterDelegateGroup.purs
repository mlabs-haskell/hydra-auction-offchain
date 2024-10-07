module HydraAuctionOffchain.Contract.RegisterDelegateGroup
  ( RegisterDelegateGroupError
      ( RegisterDelegateGroup_Error_CouldNotGetOwnPubKeyHash
      , RegisterDelegateGroup_Error_CouldNotGetWalletUtxos
      , RegisterDelegateGroup_Error_CouldNotSelectNonceUtxo
      , RegisterDelegateGroup_Error_EmptyNonceUtxoMap
      )
  , RegisterDelegateGroupContractOutput(RegisterDelegateGroupContractOutput)
  , RegisterDelegateGroupContractParams(RegisterDelegateGroupContractParams)
  , RegisterDelegateGroupContractResult
  , registerDelegateGroupContract
  , registerDelegateGroupContractErr
  ) where

import Contract.Prelude

import Cardano.ToData (toData)
import Cardano.Types (PlutusData, RedeemerDatum, ScriptHash, TransactionHash, Value)
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.Int (one) as Cardano.Int
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (plutusMintingPolicy, unspentOutputs) as Lookups
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustMintCurrencyWithRedeemer
  , mustPayToScript
  , mustSpendPubKeyOutput
  ) as Constraints
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHashes)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, singleton) as Array
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Profunctor (wrapIso)
import HydraAuctionOffchain.Codec (txHashCodec)
import HydraAuctionOffchain.Contract.AnnounceAuction (selectUtxos)
import HydraAuctionOffchain.Contract.MintingPolicies.DelegateGroup
  ( delegateGroupTokenName
  , mkDelegateGroupMintingPolicy
  )
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , ContractOutput
  , ContractResult'
  , DelegateGroupInfo
  , DelegateInfo
  , delegateGroupInfoCodec
  , delegateInfoCodec
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  )
import HydraAuctionOffchain.Contract.Types.Plutus.Redeemers
  ( DelegateGroupPolicyRedeemer(MintDelegateGroup)
  )
import HydraAuctionOffchain.Contract.Validators (mkDelegateGroupMetadataValidator)
import HydraAuctionOffchain.Lib.Codec (class HasJson)

newtype RegisterDelegateGroupContractParams = RegisterDelegateGroupContractParams
  { delegateGroupServers :: DelegateInfo
  , delegateGroupMetadata :: String
  }

derive instance Generic RegisterDelegateGroupContractParams _
derive instance Newtype RegisterDelegateGroupContractParams _
derive instance Eq RegisterDelegateGroupContractParams

instance Show RegisterDelegateGroupContractParams where
  show = genericShow

instance HasJson RegisterDelegateGroupContractParams anyParams where
  jsonCodec _ = const registerDelegateGroupContractParamsCodec

registerDelegateGroupContractParamsCodec :: CA.JsonCodec RegisterDelegateGroupContractParams
registerDelegateGroupContractParamsCodec =
  wrapIso RegisterDelegateGroupContractParams $
    CA.object "RegisterDelegateGroupContractParams"
      ( CAR.record
          { delegateGroupServers: delegateInfoCodec
          , delegateGroupMetadata: CA.string
          }
      )

newtype RegisterDelegateGroupContractOutput = RegisterDelegateGroupContractOutput
  { txHash :: TransactionHash
  , delegateGroupInfo :: DelegateGroupInfo
  }

derive instance Generic RegisterDelegateGroupContractOutput _

derive instance Newtype RegisterDelegateGroupContractOutput _
derive instance Eq RegisterDelegateGroupContractOutput

instance Show RegisterDelegateGroupContractOutput where
  show = genericShow

instance HasJson RegisterDelegateGroupContractOutput anyParams where
  jsonCodec _ = const registerDelegateGroupContractOutputCodec

registerDelegateGroupContractOutputCodec :: CA.JsonCodec RegisterDelegateGroupContractOutput
registerDelegateGroupContractOutputCodec =
  wrapIso RegisterDelegateGroupContractOutput $ CA.object "RegisterDelegateGroupContractOutput"
    $
      CAR.record
        { txHash: txHashCodec
        , delegateGroupInfo: delegateGroupInfoCodec
        }

type RegisterDelegateGroupContractResult = ContractResult'
  (delegateGroupInfo :: DelegateGroupInfo)

registerDelegateGroupContract
  :: RegisterDelegateGroupContractParams
  -> Contract (ContractOutput RegisterDelegateGroupContractOutput)
registerDelegateGroupContract =
  mkContractOutput resultToOutput
    <<< registerDelegateGroupContractErr
  where
  resultToOutput :: RegisterDelegateGroupContractResult -> RegisterDelegateGroupContractOutput
  resultToOutput rec = wrap
    { txHash: rec.txHash
    , delegateGroupInfo: rec.delegateGroupInfo
    }

registerDelegateGroupContractErr
  :: RegisterDelegateGroupContractParams
  -> ExceptT RegisterDelegateGroupError Contract RegisterDelegateGroupContractResult
registerDelegateGroupContractErr (RegisterDelegateGroupContractParams params) = do
  -- Get own public key hash to be used as the sole delegate group master key.
  -- Note: In real-world applications, the number of master keys should ideally
  -- match the number of delegates.
  ownPkh <- (Array.head <$> ownPaymentPubKeyHashes)
    !? RegisterDelegateGroup_Error_CouldNotGetOwnPubKeyHash

  -- Select nonce utxo:
  walletUtxos <- getWalletUtxos !? RegisterDelegateGroup_Error_CouldNotGetWalletUtxos
  nonceUtxo <- do
    utxos <- selectUtxos walletUtxos (Value.lovelaceValueOf BigNum.one)
      !? RegisterDelegateGroup_Error_CouldNotSelectNonceUtxo
    Array.head (Map.toUnfoldable utxos) ?? RegisterDelegateGroup_Error_EmptyNonceUtxoMap
  let nonceOref = fst nonceUtxo

  -- Get delegate group minting policy:
  delegateGroupMetadataSh <- lift $ PlutusScript.hash <$> mkDelegateGroupMetadataValidator
  delegateGroupMp <- lift $ mkDelegateGroupMintingPolicy delegateGroupMetadataSh nonceOref
  let
    mintDelegateGroupRedeemer :: RedeemerDatum
    mintDelegateGroupRedeemer = wrap $ toData MintDelegateGroup

    delegateGroupCs :: ScriptHash
    delegateGroupCs = PlutusScript.hash delegateGroupMp

    delegateGroupInfo :: DelegateGroupInfo
    delegateGroupInfo = wrap
      { delegateGroupId: delegateGroupCs
      , delegateGroupMasterKeys: Array.singleton $ unwrap ownPkh
      , delegateGroupServers: params.delegateGroupServers
      , delegateGroupMetadata: params.delegateGroupMetadata
      }

    delegateGroupInfoDatum :: PlutusData
    delegateGroupInfoDatum = toData delegateGroupInfo

    delegateGroupTokenValue :: Value
    delegateGroupTokenValue =
      Value.singleton delegateGroupCs delegateGroupTokenName BigNum.one

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend nonce utxo:
        Constraints.mustSpendPubKeyOutput nonceOref

      , -- Mint delegate group metadata token:
        Constraints.mustMintCurrencyWithRedeemer delegateGroupCs mintDelegateGroupRedeemer
          delegateGroupTokenName
          Cardano.Int.one

      -- Lock DelegateGroupInfo along with the delegate group token
      -- at the delegate group metadata validator address:
      , Constraints.mustPayToScript delegateGroupMetadataSh delegateGroupInfoDatum DatumInline
          delegateGroupTokenValue

      , -- The transaction must be signed by all master keys:
        foldMap (Constraints.mustBeSignedBy <<< wrap)
          (unwrap delegateGroupInfo).delegateGroupMasterKeys
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.plutusMintingPolicy delegateGroupMp
      , Lookups.unspentOutputs $ Map.fromFoldable [ nonceUtxo ]
      ]

  lift $ submitTxReturningContractResult { delegateGroupInfo } $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

----------------------------------------------------------------------
-- Errors

data RegisterDelegateGroupError
  = RegisterDelegateGroup_Error_CouldNotGetOwnPubKeyHash
  | RegisterDelegateGroup_Error_CouldNotGetWalletUtxos
  | RegisterDelegateGroup_Error_CouldNotSelectNonceUtxo
  | RegisterDelegateGroup_Error_EmptyNonceUtxoMap

derive instance Generic RegisterDelegateGroupError _

instance Show RegisterDelegateGroupError where
  show = genericShow

instance ToContractError RegisterDelegateGroupError where
  errorCodePrefix = const "RegisterDelegateGroup"
  errorMessage = case _ of
    RegisterDelegateGroup_Error_CouldNotGetOwnPubKeyHash ->
      "Could not get own payment public key hash."

    RegisterDelegateGroup_Error_CouldNotGetWalletUtxos ->
      "Could not get wallet utxos."

    RegisterDelegateGroup_Error_CouldNotSelectNonceUtxo ->
      "Could not select nonce utxo for delegate group minting policy."

    RegisterDelegateGroup_Error_EmptyNonceUtxoMap ->
      "Impossible: Nonce utxo map cannot be empty."
