{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data.Category where

import           GHC.Generics
import           Data.Aeson

data Category
  = Category
  { categoryId :: Int
  , categoryName :: String
  } deriving (Generic)

instance Show Category where
  show = categoryName

instance Eq Category where
  (Category x _) == (Category y _) = x == y

instance FromJSON Category
instance ToJSON Category
