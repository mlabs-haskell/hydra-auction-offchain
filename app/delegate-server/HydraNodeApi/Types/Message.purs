module DelegateServer.HydraNodeApi.Types.Message
  ( GreetingsMessage
  , HeadClosedMessage
  , HeadFinalizedMessage
  , HeadOpenMessage
  , HydraNodeApi_InMessage
      ( In_Greetings
      , In_PeerConnected
      , In_PeerDisconnected
      , In_HeadIsInitializing
      , In_Committed
      , In_HeadIsOpen
      , In_SnapshotConfirmed
      , In_TxInvalid
      , In_HeadIsClosed
      , In_ReadyToFanout
      , In_HeadIsFinalized
      )
  , HydraNodeApi_OutMessage
      ( Out_Init
      , Out_NewTx
      , Out_Close
      , Out_Contest
      , Out_Fanout
      )
  , NewTxMessage
  , PeerConnMessage
  , SnapshotConfirmedMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  ) where

import Prelude

import Contract.Transaction (Transaction)
import Data.Codec.Argonaut (JsonCodec, int, object, string) as CA
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
  | In_TxInvalid
  | In_HeadIsClosed HeadClosedMessage
  | In_ReadyToFanout
  | In_HeadIsFinalized HeadFinalizedMessage

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
          , "TxInvalid": Left unit
          , "HeadIsClosed": Right headClosedMessageCodec
          , "ReadyToFanout": Left unit
          , "HeadIsFinalized": Right headFinalizedMessageCodec
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
    In_TxInvalid ->
      Variant.inj (Proxy :: Proxy "TxInvalid") unit
    In_HeadIsClosed rec ->
      Variant.inj (Proxy :: Proxy "HeadIsClosed") rec
    In_ReadyToFanout ->
      Variant.inj (Proxy :: Proxy "ReadyToFanout") unit
    In_HeadIsFinalized rec ->
      Variant.inj (Proxy :: Proxy "HeadIsFinalized") rec

  fromVariant = Variant.match
    { "Greetings": In_Greetings
    , "PeerConnected": In_PeerConnected
    , "PeerDisconnected": In_PeerDisconnected
    , "HeadIsInitializing": const In_HeadIsInitializing
    , "Committed": const In_Committed
    , "HeadIsOpen": In_HeadIsOpen
    , "SnapshotConfirmed": In_SnapshotConfirmed
    , "TxInvalid": const In_TxInvalid
    , "HeadIsClosed": In_HeadIsClosed
    , "ReadyToFanout": const In_ReadyToFanout
    , "HeadIsFinalized": In_HeadIsFinalized
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

type HeadClosedMessage =
  { snapshotNumber :: Int
  }

headClosedMessageCodec :: CA.JsonCodec HeadClosedMessage
headClosedMessageCodec =
  CA.object "HeadClosedMessage" $ CAR.record
    { snapshotNumber: CA.int
    }

type HeadFinalizedMessage =
  { utxo :: HydraUtxoMap
  }

headFinalizedMessageCodec :: CA.JsonCodec HeadFinalizedMessage
headFinalizedMessageCodec =
  CA.object "HeadFinalizedMessage" $ CAR.record
    { utxo: hydraUtxoMapCodec
    }

----------------------------------------------------------------------
-- Outcoming messages

data HydraNodeApi_OutMessage
  = Out_Init
  | Out_NewTx NewTxMessage
  | Out_Close
  | Out_Contest
  | Out_Fanout

hydraNodeApiOutMessageCodec :: CA.JsonCodec HydraNodeApi_OutMessage
hydraNodeApiOutMessageCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Init": Left unit
          , "NewTx": Right newTxMessageCodec
          , "Close": Left unit
          , "Contest": Left unit
          , "Fanout": Left unit
          }
      )
  where
  toVariant = case _ of
    Out_Init ->
      Variant.inj (Proxy :: Proxy "Init") unit
    Out_NewTx rec ->
      Variant.inj (Proxy :: Proxy "NewTx") rec
    Out_Close ->
      Variant.inj (Proxy :: Proxy "Close") unit
    Out_Contest ->
      Variant.inj (Proxy :: Proxy "Contest") unit
    Out_Fanout ->
      Variant.inj (Proxy :: Proxy "Fanout") unit

  fromVariant = Variant.match
    { "Init": const Out_Init
    , "NewTx": Out_NewTx
    , "Close": const Out_Close
    , "Contest": const Out_Contest
    , "Fanout": const Out_Fanout
    }

type NewTxMessage =
  { transaction :: Transaction
  }

newTxMessageCodec :: CA.JsonCodec NewTxMessage
newTxMessageCodec =
  CA.object "NewTxMessage" $ CAR.record
    { transaction: txCodec
    }
