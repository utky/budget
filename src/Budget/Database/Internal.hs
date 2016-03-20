{-# LANGUAGE MultiParamTypeClasses #-}
module Budget.Database.Internal where

import           Prelude hiding (pi)
import           Data.Time (Day, LocalTime)
import           Database.Relational.Query (SqlProjectable, unsafeProjectSqlTerms, showConstantTermsSQL)

unsafeSQLiteDayValue' :: Day -> [String]
unsafeSQLiteDayValue' = showConstantTermsSQL . show

unsafeSQLiteTimeValue' :: LocalTime -> [String]
unsafeSQLiteTimeValue' = showConstantTermsSQL . show

unsafeSQLiteDayStr :: SqlProjectable p => String -> p Day
unsafeSQLiteDayStr = unsafeProjectSqlTerms . showConstantTermsSQL

unsafeSQLiteDayValue :: SqlProjectable p => Day -> p Day
unsafeSQLiteDayValue = unsafeProjectSqlTerms . unsafeSQLiteDayValue'

unsafeSQLiteTimeValue :: SqlProjectable p => LocalTime -> p LocalTime
unsafeSQLiteTimeValue = unsafeProjectSqlTerms . unsafeSQLiteTimeValue'

class To a b where
  to :: a -> b

class From a b where
  from :: b -> a
