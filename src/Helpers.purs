module HydraAuctionOffchain.Helpers
  ( dateTimeFromPosixTimeUnsafe
  , errV
  , exceptNoteE
  , fromJustWithErr
  , getInlineDatum
  , getTxOutsAt
  , liftEitherShow
  , mkPosixTimeUnsafe
  , nowPosix
  , randomElem
  , tokenNameFromAsciiUnsafe
  , waitSeconds
  , withEmptyPlutusV2Script
  , withoutRefScript
  , (!*)
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, Datum(Datum), OutputDatum(OutputDatum), fromData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts (PlutusScript(PlutusScript))
import Contract.Time (POSIXTime)
import Contract.Transaction
  ( Language(PlutusV2)
  , ScriptRef(PlutusScriptRef)
  , TransactionOutput
  , TransactionOutputWithRefScript
  )
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, mkTokenName)
import Control.Error.Util (hush, (!?))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftEither, try)
import Control.Monad.Except (ExceptT)
import Ctl.Internal.Plutus.Types.Address (class PlutusAddress)
import Data.Array (unsafeIndex)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Either (Either)
import Data.Foldable (length)
import Data.Int (toNumber) as Int
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe')
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (class Duration, Seconds(Seconds), fromDuration)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Effect.Now (now)
import Effect.Random (randomInt)
import JS.BigInt (fromNumber, toNumber) as BigInt
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

dateTimeFromPosixTimeUnsafe :: POSIXTime -> DateTime
dateTimeFromPosixTimeUnsafe =
  toDateTime
    <<< fromJustWithErr "dateTimeFromPosixTimeUnsafe"
    <<< instant
    <<< wrap
    <<< BigInt.toNumber
    <<< unwrap

tokenNameFromAsciiUnsafe :: String -> TokenName
tokenNameFromAsciiUnsafe tokenName =
  unsafePartial fromJust $ mkTokenName =<< byteArrayFromAscii tokenName

liftEitherShow :: forall m e a. MonadThrow Error m => Show e => Either e a -> m a
liftEitherShow = liftEither <<< lmap (error <<< show)

exceptNoteE :: forall a e e' m. MonadError e' m => m a -> e -> ExceptT e m a
exceptNoteE action err = (hush <$> try action) !? err

infixl 9 exceptNoteE as !*

fromJustWithErr :: forall a. String -> Maybe a -> a
fromJustWithErr message = fromMaybe' (\_ -> unsafeCrashWith $ "fromJust: " <> message)

errV :: forall e. Boolean -> e -> V (Array e) Unit
errV x error = if x then pure unit else invalid [ error ]

getInlineDatum :: forall datum. FromData datum => TransactionOutput -> Maybe datum
getInlineDatum txOut =
  case (unwrap txOut).datum of
    OutputDatum (Datum plutusData) -> fromData plutusData
    _ -> Nothing

getTxOutsAt :: forall addr. PlutusAddress addr => addr -> Contract (Array TransactionOutput)
getTxOutsAt =
  map (map (_.output <<< unwrap <<< snd) <<< Map.toUnfoldable)
    <<< utxosAt

mkPosixTimeUnsafe :: forall (a :: Type). Duration a => a -> POSIXTime
mkPosixTimeUnsafe dur =
  fromJustWithErr "mkPosixTimeUnsafe"
    (map wrap $ BigInt.fromNumber $ unwrap $ fromDuration dur)

nowPosix :: forall (m :: Type -> Type). MonadEffect m => m POSIXTime
nowPosix = liftEffect $ now <#> mkPosixTimeUnsafe <<< unInstant

randomElem :: forall m a. MonadEffect m => Array a -> m a
randomElem xs =
  liftEffect $ unsafePartial $
    unsafeIndex xs <$> randomInt zero (length xs - one)

waitSeconds :: forall m. MonadAff m => Int -> m Unit
waitSeconds seconds = liftAff $ delay $ fromDuration $ Seconds $ Int.toNumber seconds

withoutRefScript :: TransactionOutput -> TransactionOutputWithRefScript
withoutRefScript output = wrap
  { output
  , scriptRef: Nothing
  }

withEmptyPlutusV2Script :: TransactionOutput -> TransactionOutputWithRefScript
withEmptyPlutusV2Script output = wrap
  { output
  , scriptRef: Just scriptRefEmpty
  }
  where
  scriptRefEmpty :: ScriptRef
  scriptRefEmpty =
    PlutusScriptRef $ PlutusScript (mempty /\ PlutusV2)
