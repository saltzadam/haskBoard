{-# LANGUAGE DeriveAnyClass #-}

module NumberedPiece (NumberedPiece (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), ToJSONKey (..), FromJSONKey (..))
import Data.Aeson.Types (FromJSONKeyFunction (..), toJSONKeyText)
import Data.Finitary (Finitary)
import Data.Finite (Finite, finite, getFinite)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | A resource identified by a number in [0, n).
-- Use this instead of raw 'Finite n': it serialises as a plain JSON number
-- and has a clean Show instance (displays as the integer).
newtype NumberedPiece n = NumberedPiece (Finite n)
  deriving (Eq, Ord, Generic, Finitary)

instance Show (NumberedPiece n) where
  show (NumberedPiece i) = show (getFinite i)

instance KnownNat n => Read (NumberedPiece n) where
  readsPrec p s = [(NumberedPiece (finite i), rest) | (i, rest) <- readsPrec p s]

instance ToJSON (NumberedPiece n) where
  toJSON (NumberedPiece i) = toJSON (getFinite i)

instance KnownNat n => FromJSON (NumberedPiece n) where
  parseJSON v = NumberedPiece . finite <$> parseJSON v

instance ToJSONKey (NumberedPiece n) where
  toJSONKey = toJSONKeyText (\(NumberedPiece i) -> T.pack . show . getFinite $ i)

instance KnownNat n => FromJSONKey (NumberedPiece n) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case readMaybe (T.unpack t) of
      Just i  -> pure (NumberedPiece (finite i))
      Nothing -> fail $ "NumberedPiece: could not parse key: " ++ T.unpack t
