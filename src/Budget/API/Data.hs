{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Budget.API.Data where

import           Control.Monad (MonadPlus, mzero)
import           Data.List (intercalate)
import           Data.Attoparsec.Text
import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import           Servant.Common.Text (FromText(..))

dateSep :: Parser Char
dateSep = char '-'

year :: Parser Year
year = Year . read <$> count 4 digit

month :: Parser Month
month =
  let month' = read <$> (count 2 digit)
      mtuple = (,) <$> year <* dateSep <*> month'
  in  Month <$> mtuple

day :: Parser Day
day =
  let day' = read <$> (count 2 digit)
      dtuple = (,) <$> month <* dateSep <*> day'
  in  Day <$> dtuple

execParse :: (MonadPlus m) => Parser a -> Text -> m a
execParse p = either (const mzero) return . eitherResult . parse p 

execParseFromText :: Parser a -> Text -> Maybe a
execParseFromText p = either (const Nothing) Just . eitherResult . parse p 

newtype Year = Year Integer deriving (Generic, Show)
instance ToJSON Year 
instance FromJSON Year
instance FromText Year

newtype Month = Month (Year, Int) deriving (Generic, Show)
instance ToJSON Month where
    toJSON (Month (y, m)) = toJSON $ intercalate "-" [(show y), (show m)]

instance FromJSON Month where
    parseJSON (String t) = execParse month t
    parseJSON _ = mzero

instance FromText Month where
    fromText = execParseFromText month

newtype Day = Day (Month, Int) deriving (Generic, Show)
instance ToJSON Day where
    toJSON (Day ((Month (y, m)), d)) = toJSON $ intercalate "-" [(show y), (show m), (show d)]
instance FromJSON Day where
    parseJSON (String t) = execParse day t
    parseJSON _ = mzero

instance FromText Day where
    fromText = execParseFromText day
