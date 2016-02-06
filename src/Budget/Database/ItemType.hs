{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.ItemType where

import           Budget.Database.Schema (defineTable)

$(defineTable "item_type")

