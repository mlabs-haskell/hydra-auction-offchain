module HydraAuctionOffchain.Lib.Cose
  ( getCoseSign1Signature
  , mkSigStructure
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types (Address, NetworkId)
import Contract.Prim.ByteArray (ByteArray, CborBytes)
import Effect (Effect)

foreign import getCoseSign1Signature :: ByteArray -> Effect ByteArray

foreign import newSigStructure :: ByteArray -> ProtectedHeaderMap -> ByteArray

foreign import data ProtectedHeaderMap :: Type
foreign import newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap

foreign import data HeaderMap :: Type
foreign import newHeaderMap :: Effect HeaderMap
foreign import setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
foreign import setAddressHeader :: CborBytes -> HeaderMap -> Effect Unit

mkSigStructure :: Address -> ByteArray -> Effect ByteArray
mkSigStructure address payload =
  newSigStructure payload <$> headers
  where
  headers :: Effect ProtectedHeaderMap
  headers = do
    headerMap <- newHeaderMap
    setAlgHeaderToEdDsa headerMap
    setAddressHeader (encodeCbor address) headerMap
    pure $ newProtectedHeaderMap headerMap
