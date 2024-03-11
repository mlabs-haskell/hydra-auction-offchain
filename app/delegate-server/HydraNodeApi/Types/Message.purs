module DelegateServer.HydraNodeApi.Types.Message
  ( GreetingsMessage
  , HeadOpenMessage
  , HydraNodeApi_InMessage
      ( In_Greetings
      , In_PeerConnected
      , In_PeerDisconnected
      , In_HeadIsInitializing
      , In_Committed
      , In_HeadIsOpen
      , In_SnapshotConfirmed
      )
  , HydraNodeApi_OutMessage
      ( Out_Init
      , Out_NewTx
      )
  , NewTxMessage
  , PeerConnMessage
  , SnapshotConfirmedMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  ) where

import Prelude

import Contract.Transaction (Transaction)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (optional, record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe)
import Data.Profunctor (dimap)
import Data.Variant (inj, match) as Variant
import DelegateServer.Lib.Codec (fixTaggedSumCodec, txCodec)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus, headStatusCodec)
import DelegateServer.Types.HydraSnapshot (HydraSnapshot, hydraSnapshotCodec)
import DelegateServer.Types.HydraUtxoMap (HydraUtxoMap, hydraUtxoMapCodec)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- Incoming messages

data HydraNodeApi_InMessage
  = In_Greetings GreetingsMessage
  | In_PeerConnected PeerConnMessage
  | In_PeerDisconnected PeerConnMessage
  | In_HeadIsInitializing
  | In_Committed
  | In_HeadIsOpen HeadOpenMessage
  | In_SnapshotConfirmed SnapshotConfirmedMessage

hydraNodeApiInMessageCodec :: CA.JsonCodec HydraNodeApi_InMessage
hydraNodeApiInMessageCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Greetings": Right greetingsMessageCodec
          , "PeerConnected": Right peerConnMessageCodec
          , "PeerDisconnected": Right peerConnMessageCodec
          , "HeadIsInitializing": Left unit
          , "Committed": Left unit
          , "HeadIsOpen": Right headOpenMessageCodec
          , "SnapshotConfirmed": Right snapshotConfirmedMessageCodec
          }
      )
  where
  toVariant = case _ of
    In_Greetings rec ->
      Variant.inj (Proxy :: Proxy "Greetings") rec
    In_PeerConnected rec ->
      Variant.inj (Proxy :: Proxy "PeerConnected") rec
    In_PeerDisconnected rec ->
      Variant.inj (Proxy :: Proxy "PeerDisconnected") rec
    In_HeadIsInitializing ->
      Variant.inj (Proxy :: Proxy "HeadIsInitializing") unit
    In_Committed ->
      Variant.inj (Proxy :: Proxy "Committed") unit
    In_HeadIsOpen rec ->
      Variant.inj (Proxy :: Proxy "HeadIsOpen") rec
    In_SnapshotConfirmed rec ->
      Variant.inj (Proxy :: Proxy "SnapshotConfirmed") rec

  fromVariant = Variant.match
    { "Greetings": In_Greetings
    , "PeerConnected": In_PeerConnected
    , "PeerDisconnected": In_PeerDisconnected
    , "HeadIsInitializing": const In_HeadIsInitializing
    , "Committed": const In_Committed
    , "HeadIsOpen": In_HeadIsOpen
    , "SnapshotConfirmed": In_SnapshotConfirmed
    }

type PeerConnMessage =
  { peer :: String
  }

peerConnMessageCodec :: CA.JsonCodec PeerConnMessage
peerConnMessageCodec =
  CA.object "PeerConnMessage" $ CAR.record
    { peer: CA.string
    }

type GreetingsMessage =
  { headStatus :: HydraHeadStatus
  , snapshotUtxo :: Maybe HydraUtxoMap
  }

greetingsMessageCodec :: CA.JsonCodec GreetingsMessage
greetingsMessageCodec =
  CA.object "GreetingsMessage" $ CAR.record
    { headStatus: headStatusCodec
    , snapshotUtxo: CAR.optional hydraUtxoMapCodec
    }

type HeadOpenMessage =
  { utxo :: HydraUtxoMap
  }

headOpenMessageCodec :: CA.JsonCodec HeadOpenMessage
headOpenMessageCodec =
  CA.object "HeadOpenMessage" $ CAR.record
    { utxo: hydraUtxoMapCodec
    }

type SnapshotConfirmedMessage =
  { snapshot :: HydraSnapshot
  }

snapshotConfirmedMessageCodec :: CA.JsonCodec SnapshotConfirmedMessage
snapshotConfirmedMessageCodec =
  CA.object "SnapshotConfirmedMessage" $ CAR.record
    { snapshot: hydraSnapshotCodec
    }

----------------------------------------------------------------------
-- Outcoming messages

data HydraNodeApi_OutMessage
  = Out_Init
  | Out_NewTx NewTxMessage

hydraNodeApiOutMessageCodec :: CA.JsonCodec HydraNodeApi_OutMessage
hydraNodeApiOutMessageCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Init": Left unit
          , "NewTx": Right newTxMessageCodec
          }
      )
  where
  toVariant = case _ of
    Out_Init ->
      Variant.inj (Proxy :: Proxy "Init") unit
    Out_NewTx rec ->
      Variant.inj (Proxy :: Proxy "NewTx") rec

  fromVariant = Variant.match
    { "Init": const Out_Init
    , "NewTx": Out_NewTx
    }

type NewTxMessage =
  { transaction :: Transaction
  }

newTxMessageCodec :: CA.JsonCodec NewTxMessage
newTxMessageCodec =
  CA.object "NewTxMessage" $ CAR.record
    { transaction: txCodec
    }
