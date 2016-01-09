{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.Expense where

import           Budget.Database.Schema (defineTable)

$(defineTable "expense")

