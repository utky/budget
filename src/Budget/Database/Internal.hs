{-# LANGUAGE MultiParamTypeClasses #-}
module Budget.Database.Internal where

import           Data.Time (Day, LocalTime)
import           Database.Relational.Query (SqlProjectable, unsafeProjectSqlTerms, showConstantTermsSQL)

unsafeSQLiteDayValue :: SqlProjectable p => Day -> p Day
unsafeSQLiteDayValue = unsafeProjectSqlTerms . showConstantTermsSQL . show

unsafeSQLiteTimeValue :: SqlProjectable p => LocalTime -> p LocalTime
unsafeSQLiteTimeValue = unsafeProjectSqlTerms . showConstantTermsSQL . show

class To a b where
  to :: a -> b

class From a b where
  from :: b -> a
