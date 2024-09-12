module HydraAuctionOffchain.Lib.Plutus.Address
  ( toPubKeyHash
  ) where

import Prelude

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential)) as Plutus
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash) as Plutus
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)

toPubKeyHash :: Plutus.Address -> Maybe Plutus.PubKeyHash
toPubKeyHash addr =
  case (unwrap addr).addressCredential of
    Plutus.PubKeyCredential pkh -> Just pkh
    _ -> Nothing
