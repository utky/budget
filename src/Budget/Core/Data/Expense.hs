{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data.Expense where

import           Prelude hiding (id)
import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import           Budget.Core.Data.Amount
import           Budget.Core.Data.Category
import           Budget.Core.Data.Date

data Expense
  = Expense
  { id       :: Int -- ^ Identifier of item
  , name     :: Text -- ^ Name of item
  , date     :: Date -- ^ Date of 
  , note     :: Text -- ^ Note
  , amount   :: Amount
  , category :: Category
  } deriving (Generic)

instance Eq Expense where
  x == y = id x == id y

instance Ord Expense where
  a `compare` b = amount a `compare` amount b

instance FromJSON Expense
instance ToJSON Expense
