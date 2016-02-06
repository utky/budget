{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.ItemTemplate where

import           Budget.Database.Schema (defineTable)

$(defineTable "item_template")

