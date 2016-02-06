{-# LANGUAGE Rank2Types #-}
module Budget.Core.Store where

import           Data.Text (Text)
import           Control.Monad.Free
import           Budget.Core.Data
import           Data.Time

data ByMonth = ByMonth { byYear :: Integer, byMonth :: Int }

data BetweenDay = BetweenDay { fromDay :: Day, toDay :: Day }

data Store a 
  -- | Persist @p@ as new data and emit value @a@.
  = New a NewQ
  -- | Save @p@ and emit value @a@.
  | Update a UpdateQ
  -- | Evict @p@ from store.
  | Remove a RemoveQ
  -- | Fetch @a@ from store.
  | Fetch (FetchQ a)

instance Functor Store where
  fmap f (New a p) = New (f a) p
  fmap f (Update a p) = Update (f a) p
  fmap f (Remove a p) = Remove (f a) p
  fmap f (Fetch q) = Fetch (fmap f q) 

type StoreM = Free Store

liftS :: Store a -> StoreM a
liftS = liftF

foldStoreM :: (Monad m) => (forall x. Store x -> m x) -> StoreM a -> m a
foldStoreM = foldFree

-- | Domain specific queries for create data.
data NewQ 
  = NewIncome NewIncomeR
  | NewExpense NewExpenseR
  | NewIncomeCategory Category
  | NewExpenseCategory Category

data UpdateQ
  = UpdateIncome
  | UpdateExpense

data RemoveQ
  = RemoveIncome
  | RemoveExpense

data FetchQ a
  = IncomeCategories  ([Category] -> a)
  | ExpenseCategories ([Category] -> a)
  | IncomeByMonth     ([Income] -> a)   ByMonth
  | ExpenseByMonth    ([Expense] -> a)  ByMonth

incomeCategories :: FetchQ [Category]
incomeCategories = IncomeCategories id

expenseCategories :: FetchQ [Category]
expenseCategories = ExpenseCategories id

instance Functor FetchQ where
  fmap f (IncomeCategories g) = IncomeCategories (f . g)
  fmap f (ExpenseCategories g) = ExpenseCategories (f . g)
  fmap f (IncomeByMonth g month) = IncomeByMonth (f . g) month
  fmap f (ExpenseByMonth g month) = ExpenseByMonth (f . g) month
