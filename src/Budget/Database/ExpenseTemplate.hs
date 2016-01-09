{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.ExpenseTemplate where

import           Budget.Database.Schema (defineTable)

$(defineTable "expense_template")

