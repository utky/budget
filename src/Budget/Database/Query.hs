{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Budget.Database.Query where

import qualified Budget.Core as Core
import           Budget.Database.Schema (schema)
import           Budget.Database.Internal (Iso(..))

import qualified Budget.Database.IncomeCategory as IncomeCategory
import qualified Budget.Database.ExpenseCategory as ExpenseCategory
import qualified Budget.Database.Income as Income
import qualified Budget.Database.Expense as Expense
import qualified Budget.Database.Item as Item

import           Data.Functor ((<$))
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Time (getCurrentTime)
import           Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import           Database.HDBC (IConnection, runRaw, SqlValue, withTransaction, getTables)
import           Database.Relational.Query
import           Database.Record (ToSql, FromSql)
import           Database.HDBC.Record


{-| Database operation
-}
newtype DB a = DB { unDB :: forall conn. (IConnection conn) => conn -> IO a }

instance Functor DB where
  fmap f fa = DB (\conn -> fmap f (unDB fa conn))

instance Applicative DB where
  pure a = DB $ \_ -> return a
  f <*> fa = DB (\conn -> (unDB f conn) <*> (unDB fa conn))

instance Monad DB where
  return = pure
  fa >>= f = let unwrap c = flip unDB c
             in  DB (\conn -> unwrap conn (f `fmap` fa) >>= unwrap conn)
      
instance MonadIO DB where
  liftIO action = DB (\_ -> action)

insertDB :: (ToSql SqlValue p) => Insert p -> p -> DB Integer
-- insertDB = InsertDB id
insertDB i p = DB (\conn -> runInsert conn i p)

insertQueryDB :: (ToSql SqlValue p) => InsertQuery p -> p -> DB Integer
insertQueryDB i p = DB (\conn -> runInsertQuery conn i p)

updateDB :: (ToSql SqlValue p) => Update p -> p -> DB Integer
-- updateDB = UpdateDB id
updateDB u p = DB (\conn -> runUpdate conn u p)

deleteDB :: (ToSql SqlValue p) => Delete p -> p -> DB Integer
-- deleteDB = DeleteDB id
deleteDB d p = DB (\conn -> runDelete conn d p)

selectDB :: (ToSql SqlValue p, FromSql SqlValue a) => Relation p a -> p -> DB [a]
selectDB q p = DB (\conn -> runQuery conn (relationalQuery q) p)

rawDB    :: String -> DB ()
rawDB  sql = DB (\conn -> runRaw conn sql)

tableNames :: DB [String]
tableNames = DB getTables

-- | Run database middleware and gain result with side-effect.
runDB :: ( IConnection conn , MonadIO m) => DB r -> conn -> m r
runDB db connection = liftIO (withTransaction connection (unDB db))


-- | Transform domain specific structure to middleware specific structure
runStore :: Core.Store a -> DB a
runStore (Core.New x q) = runNewQ x q
runStore (Core.Update x q) = runUpdateQ x q
runStore (Core.Remove x q) = runRemoveQ x q
runStore (Core.Fetch q) = runFetchQ q

runStoreM :: Core.StoreM a -> DB a
runStoreM = Core.foldStoreM runStore

runNewQ :: a -> Core.NewQ -> DB a
runNewQ x (Core.NewIncomeCategory cat)
  = x <$ insertQueryDB (IncomeCategory.insertFromCategory cat) ()

runNewQ x (Core.NewExpenseCategory cat)
  = x <$ insertQueryDB (ExpenseCategory.insertFromCategory cat) ()

runNewQ x (Core.NewIncome i)
  = x <$ do
    itemId <- return 1
    utcnow <- liftIO getCurrentTime
    zone <- liftIO getCurrentTimeZone
    let now = utcToLocalTime zone utcnow
    insertQueryDB (Item.insertFromIncome itemId now i) ()
    insertDB Income.insertIncome (Income.Income itemId (Core.newIncomeCategoryId i))

runNewQ _ _ = undefined

runUpdateQ :: a -> Core.UpdateQ -> DB a
runUpdateQ = undefined

runRemoveQ :: a -> Core.RemoveQ -> DB a
runRemoveQ = undefined

{-| Mapping from FetchQ to Query
-}
runFetchQ :: Core.FetchQ a -> DB a
runFetchQ (Core.IncomeCategories f)
  = fmap (f . (map from)) (selectDB IncomeCategory.incomeCategory ())
runFetchQ (Core.ExpenseCategories f)
  = fmap (f . (map from)) (selectDB ExpenseCategory.expenseCategory ())
runFetchQ (Core.ExpenseByMonth f p)
  = undefined
runFetchQ (Core.IncomeByMonth f p)
  = undefined

