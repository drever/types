{-# LANGUAGE DeriveFunctor #-}

import Data.Fix
import Prelude hiding (length, succ)

data ListF a b = Nil
               | Cons a b deriving (Show, Functor)

type List a = Fix (ListF a)
-- ^ mit `Fix` verschwindet der freie Typparameter. Daher ist es so aehnlich wie ein Quantor, also ein existential Type.
-- Bei dem existential Type wird auch eine freie Typenvariable gebunden.

nil :: List a
nil = Fix Nil

cons :: a -> List a -> List a
cons a as = Fix (Cons a as)

mylist :: List Int
mylist = cons 3 (cons 4 (cons 5 nil))

length :: List a -> Int
length = foldFix $ \x -> case x of
             Nil -> 0
             Cons _ n -> n + 1

-- Mu

type Nat = Mu Maybe

zero = Mu $ \f -> f Nothing
succ n = Mu (\f -> f (Just (foldMu f n)))

five = succ $ succ $ succ $ succ $ succ $ zero
three = succ $ succ $ succ $ zero

add :: Nat -> Nat -> Nat
add m = foldMu (maybe m succ)

intToNat :: Int -> Nat
intToNat x = if x >= 0
   then case x of
          0 -> zero
          n -> succ (intToNat $ n - 1)
   else undefined

natToInt :: Nat -> Int
natToInt = foldMu $ maybe 0 (+1)
