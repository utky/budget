module Budget.Core.Data.Date where

import           Control.Monad (mzero)
import           Data.Time
import           Data.Time.Calendar (addGregorianMonthsClip)
import           Data.Aeson
import           Data.Text (unpack)

type Date = Day

instance ToJSON Day where
    toJSON d = toJSON (show d)
    
instance FromJSON Day where
    parseJSON (String s)
      = case unpack s of
          (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:[]) -> return $ mkDate (read (y1:y2:y3:y4:[])) (read (m1:m2:[])) (read (d1:d2:[]))
          _ -> mzero
    parseJSON _ = mzero
 

mkDate :: Integer -> Int -> Int -> Date
mkDate = fromGregorian

nextMonth :: Date -> Date
nextMonth date = addGregorianMonthsClip 1 date

