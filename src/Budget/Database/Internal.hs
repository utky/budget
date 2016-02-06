{-# LANGUAGE MultiParamTypeClasses #-}
module Budget.Database.Internal where


class Iso a b where
  to :: a -> b
  from :: b -> a
