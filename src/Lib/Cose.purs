module HydraAuctionOffchain.Lib.Cose
  ( getCoseSign1Signature
  , mkSigStructure
  ) where

import Prelude

import Contract.Address (Address)
import Contract.Config (NetworkId)
import Contract.Prim.ByteArray (ByteArray, CborBytes)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Ctl.Internal.Serialization (toBytes)
import Effect (Effect)

foreign import getCoseSign1Signature :: ByteArray -> Effect ByteArray

foreign import newSigStructure :: ByteArray -> ProtectedHeaderMap -> ByteArray

foreign import data ProtectedHeaderMap :: Type
foreign import newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap

foreign import data HeaderMap :: Type
foreign import newHeaderMap :: Effect HeaderMap
foreign import setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
foreign import setAddressHeader :: CborBytes -> HeaderMap -> Effect Unit

mkSigStructure :: NetworkId -> Address -> ByteArray -> Effect ByteArray
mkSigStructure network address payload =
  newSigStructure payload <$> headers
  where
  headers :: Effect ProtectedHeaderMap
  headers = do
    headerMap <- newHeaderMap
    setAlgHeaderToEdDsa headerMap
    setAddressHeader (toBytes $ fromPlutusAddress network address) headerMap
    pure $ newProtectedHeaderMap headerMap
