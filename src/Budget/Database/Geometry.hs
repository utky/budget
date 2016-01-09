{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.Geometry where

import           Budget.Database.Schema (defineTable)

$(defineTable "geometry")

