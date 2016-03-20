{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data.ItemTemplate where

import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import qualified Budget.Core.Data.Category as C

data ItemTemplate
  = ItemTemplate
  { name     :: Text -- ^ Name of item
  , category :: C.Category -- ^ Assigned category
  } deriving (Generic)

instance Eq ItemTemplate where
  x == y = name x == name y && catid x == catid y
    where catid = C.categoryId .  category

instance FromJSON ItemTemplate
instance ToJSON ItemTemplate

data NewItemTemplateR
  = NewItemTemplateR
  { newItemTemplateName :: Text
  , newItemTemplateCategoryId :: Int
  } deriving (Generic)

instance FromJSON NewItemTemplateR
instance ToJSON NewItemTemplateR
