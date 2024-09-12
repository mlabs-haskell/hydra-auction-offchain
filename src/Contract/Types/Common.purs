module HydraAuctionOffchain.Contract.Types.Common
  ( Utxo
  ) where

import Cardano.Types (TransactionInput, TransactionOutput)
import Data.Tuple (Tuple)

type Utxo = Tuple TransactionInput TransactionOutput
