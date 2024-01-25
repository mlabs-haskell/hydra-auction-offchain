module HydraAuctionOffchain.Helpers
  ( errV
  , exceptNoteE
  , getInlineDatum
  , liftEitherShow
  , tokenNameFromAsciiUnsafe
  , (!*)
  ) where

import Prelude

import Contract.PlutusData (class FromData, Datum(Datum), OutputDatum(OutputDatum), fromData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Transaction (TransactionOutput)
import Contract.Value (TokenName, mkTokenName)
import Control.Error.Util (hush, (!?))
import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftEither, try)
import Control.Monad.Except (ExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (unwrap)
import Data.Validation.Semigroup (V, invalid)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafePartial)

tokenNameFromAsciiUnsafe :: String -> TokenName
tokenNameFromAsciiUnsafe tokenName =
  unsafePartial fromJust $ mkTokenName =<< byteArrayFromAscii tokenName

liftEitherShow :: forall m e a. MonadThrow Error m => Show e => Either e a -> m a
liftEitherShow = liftEither <<< lmap (error <<< show)

exceptNoteE :: forall a e e' m. MonadError e' m => m a -> e -> ExceptT e m a
exceptNoteE action err = (hush <$> try action) !? err

infixl 9 exceptNoteE as !*

errV :: forall e. Boolean -> e -> V (Array e) Unit
errV x error = if x then pure unit else invalid [ error ]

getInlineDatum :: forall datum. FromData datum => TransactionOutput -> Maybe datum
getInlineDatum txOut =
  case (unwrap txOut).datum of
    OutputDatum (Datum plutusData) -> fromData plutusData
    _ -> Nothing
