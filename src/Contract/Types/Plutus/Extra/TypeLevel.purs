module HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
  ( module ExportRowList
  , class FromDataRec
  , class RowListLength
  , class ToDataRec
  , AppCons
  , fromDataRec
  , recLength
  , rowListLength
  , toDataRec
  , type (:~:)
  , type (:$:)
  ) where

import Prelude

import Contract.PlutusData (class FromData, class ToData, PlutusData, fromData, toData)
import Control.Error.Util (bool)
import Data.Array (uncons, (:))
import Data.Foldable (null)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, RowList)
import Prim.RowList (Nil) as ExportRowList
import Record (delete, get, insert) as Record
import Type.Proxy (Proxy(Proxy))

infixr 1 type RowList.Cons as :~:

type AppCons :: forall k. (RowList k -> RowList k) -> RowList k -> RowList k
type AppCons f rl = f rl

infixr 0 type AppCons as :$:

--------------------------------------------------------------------------------
-- RowListLength
--------------------------------------------------------------------------------

class RowListLength :: forall k. RowList k -> Constraint
class RowListLength rl where
  rowListLength :: Proxy rl -> Int

instance RowListLength RowList.Nil where
  rowListLength _ = 0

instance RowListLength tail => RowListLength (RowList.Cons label type_ tail) where
  rowListLength _ = 1 + rowListLength (Proxy :: Proxy tail)

recLength
  :: forall r rl t
   . RowToList r rl
  => RowListLength rl
  => Newtype t (Record r)
  => Proxy t
  -> Int
recLength _ = rowListLength (Proxy :: Proxy rl)

--------------------------------------------------------------------------------
-- FromDataRec
--------------------------------------------------------------------------------

class FromDataRec :: RowList Type -> Row Type -> Constraint
class FromDataRec rl r | rl -> r where
  fromDataRec :: Proxy rl -> Array PlutusData -> Maybe (Record r)

instance FromDataRec RowList.Nil () where
  fromDataRec _ = bool Nothing (Just {}) <<< null

instance
  ( FromData t
  , FromDataRec tail rowRest
  , Lacks l rowRest
  , Cons l t rowRest rowFull
  , IsSymbol l
  ) =>
  FromDataRec (RowList.Cons l t tail) rowFull where
  fromDataRec _ pd = do
    { head: x, tail: xs } <- uncons pd
    a <- fromData x :: Maybe t
    Record.insert (Proxy :: Proxy l) a <$> fromDataRec (Proxy :: Proxy tail) xs

--------------------------------------------------------------------------------
-- ToDataRec
--------------------------------------------------------------------------------

class ToDataRec :: RowList Type -> Row Type -> Constraint
class ToDataRec rl r | rl -> r where
  toDataRec :: Proxy rl -> Record r -> Array PlutusData

instance ToDataRec RowList.Nil () where
  toDataRec _ _ = mempty

instance
  ( ToData t
  , ToDataRec tail rowRest
  , Lacks l rowRest
  , Cons l t rowRest rowFull
  , IsSymbol l
  ) =>
  ToDataRec (RowList.Cons l t tail) rowFull where
  toDataRec _ rec =
    toData (Record.get (Proxy :: Proxy l) rec)
      : toDataRec (Proxy :: Proxy tail) (Record.delete (Proxy :: Proxy l) rec)
