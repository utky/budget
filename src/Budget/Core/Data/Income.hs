{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data.Income where

import           Prelude hiding (id)
import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import           Budget.Core.Data.Amount
import           Budget.Core.Data.Category
import           Budget.Core.Data.Date

data Income
  = Income
  { id       :: Int -- ^ Identifier of item
  , name     :: Text -- ^ Name of item
  , date     :: Date -- ^ Date of 
  , note     :: Text -- ^ Note
  , amount   :: Amount
  , category :: Category
  } deriving (Generic)

instance Eq Income where
  x == y = id x == id y

instance Ord Income where
  a `compare` b = amount a `compare` amount b

instance FromJSON Income
instance ToJSON Income 
