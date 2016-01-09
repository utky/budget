{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.IncomeTemplate where

import           Budget.Database.Schema (defineTable)

$(defineTable "income_template")

