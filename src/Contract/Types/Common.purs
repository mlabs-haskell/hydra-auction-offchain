module HydraAuctionOffchain.Contract.Types.Common
  ( Utxo
  ) where

import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Data.Tuple (Tuple)

type Utxo = Tuple TransactionInput TransactionOutputWithRefScript
