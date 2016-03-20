module Budget.App (run, BudgetCommand(..)) where

import           Servant
import qualified Network.Wai.Handler.Warp as Warp
import           Budget.API (api, server, createDB)
import           Budget.Database (ConnWrapper(..), connectSqlite3)

type PortNum = Int
type DataPath = String

{-| Command line argument
-}
data BudgetCommand
  = Configure DataPath
  | Start PortNum DataPath
  deriving (Show)

connectDB :: String -> IO ConnWrapper
connectDB = fmap ConnWrapper . connectSqlite3

run :: BudgetCommand -> IO ()
run (Configure datapath) = (connectDB datapath) >>= createDB
run (Start port datapath) = startServer port datapath

startServer :: PortNum -> DataPath -> IO ()
startServer portNum datapath = do
  putStrLn $ "Listening on port " ++ (show portNum) ++ " with data " ++ datapath
  connection <- connectDB datapath
  Warp.run portNum $ serve api (server connection)
