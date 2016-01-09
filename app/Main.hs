module Main where

import           System.Environment (getArgs)
import           Budget.App (run, BudgetCommand(..))

main :: IO ()
main = do
  args <- getArgs
  case parseOpts args of
    Left m  -> putStrLn m
    Right c -> run c

parseOpts :: [String] -> Either String BudgetCommand
parseOpts ("configure":options)
  = Right (Configure (parseDatapath options))
parseOpts ("start":options)
  = Right (Start (parsePort options) (parseDatapath options))
parseOpts _
  = Left "configure | start"

parseDatapath :: [String] -> String
parseDatapath []                = ":memory:"
parseDatapath ("-d":datapath:_) = datapath
parseDatapath (_:xs)            = parseDatapath xs

parsePort :: [String] -> Int
parsePort []            = 8080
parsePort ("-p":port:_) = read port
parsePort (_:xs)        = parsePort xs
