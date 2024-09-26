module HydraAuctionOffchain.Contract.Types.Plutus.DelegateGroupInfo where

{-
import Prelude

import Cardano.Types (RawBytes, ScriptHash, URL)
import Data.ByteArray (ByteArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Undefined (undefined)
import Data.UInt (UInt)

type Url = String

newtype DelegateGroupMetadata = DelegateGroupMetadata
  { url :: Url
  , dataHash :: RawBytes 
  }

newtype DelegateGroupInfo = DelegateGroupInfo
  { groupId :: ScriptHash
  , groupMetadata :: DelegateGroupMetadata
  , delegates :: Array Ed25519KeyHash
  , httpServers :: Array Url
  , wsServers :: Array Url
  }

derive instance Generic DelegateGroupInfo _
derive instance Newtype DelegateGroupInfo _
derive instance Eq DelegateGroupInfo

instance Show DelegateGroupInfo where
  show = genericShow

-- Discover registered delegate groups. 
queryDelegateGroups :: Contract (Array DelegateGroupInfo) 
queryDelegateGroups = undefined 

type DelegateGroupSlot = Int

-- Attempt to book a delegate group slot for an upcoming auction.
-- 
-- This query will perform the following steps under the hood:
-- 1. Query a list of available auction slots from each delegate.
-- 2. Identify the common slots by finding their intersection.
-- 3. Randomly select one slot from the common available slots.
-- 4. Attempt to book the selected slot by sending booking requests to each delegate.
bookSlotForAuction :: DelegateGroupInfo -> Aff (Maybe DelegateGroupSlot)
bookSlotForAuction = undefined

newtype HostAuctionInfo = HostAuctionInfo
  { delegateGroup :: DelegateGroupInfo
  , slot :: DelegateGroupSlot
  , auctionMetadataOref :: TransactionInput
  }

-- Attempt to host announced auction at a specific delegate group slot.
hostAuction :: HostAuctionInfo -> Aff (ContractOutput Unit)
hostAuction = undefined
-}
