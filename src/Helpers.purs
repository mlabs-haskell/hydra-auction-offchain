module HydraAuctionOffchain.Helpers
  ( liftEitherShow
  , tokenNameFromAsciiUnsafe
  ) where

import Prelude

import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Value (TokenName, mkTokenName)
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (fromJust)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafePartial)

tokenNameFromAsciiUnsafe :: String -> TokenName
tokenNameFromAsciiUnsafe tokenName =
  unsafePartial fromJust $ mkTokenName =<< byteArrayFromAscii tokenName

liftEitherShow :: forall m e a. MonadThrow Error m => Show e => Either e a -> m a
liftEitherShow = liftEither <<< lmap (error <<< show)
