
{-# LANGUAGE 
  DataKinds,
  PolyKinds,
  TypeInType,
  RankNTypes, 
  TypeFamilies,
  TypeOperators,
  UndecidableInstances
 #-}

module UnitCalc where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

-- Type-level integers
data TNat = Succ TNat | One deriving (Show,Eq)
data TInt = Plus TNat | Zero | Minus TNat deriving (Show,Eq)

-- List of (id,exponent) pairs
--newtype Unit = U [(TNat,TInt)]
type Unit = [(TNat,TInt)]

--class EQ (x :: SimpleUnit) (y :: SimpleUnit) (r :: Bool) | x y -> r
--instance EQ x x True
--instance (r ~ False) => EQ x y r

type family Cmp (x::TNat) (y::TNat) :: Ordering where
    Cmp One One = EQ
    Cmp One (Succ y) = LT
    Cmp (Succ x) One = GT
    Cmp (Succ x) (Succ y) = Cmp x y

type family Negate (x::TInt) :: TInt where
    Negate Zero = Zero
    Negate (Plus x) = Minus x
    Negate (Minus x) = Plus x

type family Add (x::TInt) (y::TInt) :: TInt where
    Add Zero y = y
    Add (Minus x) y = Add (Plus x) (Negate y)
    Add (Plus (Succ x)) (Plus y) = Add (Plus x) (Plus (Succ y))
    Add (Plus One) (Plus y) = Plus (Succ y)
    Add (Plus x) Zero = Plus x
    Add (Plus (Succ x)) (Minus (Succ y)) = Add (Plus x) (Minus y)
    Add (Plus One) (Minus (Succ y)) = Minus y
    Add (Plus (Succ x)) (Minus One) = Plus x
    Add (Plus One) (Minus One) = Zero

--type family ConsUnit (a::(TNat,TInt)) (u::Unit) :: Unit where
--    ConsUnit a (U as) = U (a ': as)

type family RecipUnit (u::Unit) :: Unit where
    RecipUnit '[] = '[]
    RecipUnit ('(id,exp) ': us) = '(id,Negate exp) ': RecipUnit us

type family InsertUnit (cmp::Ordering) (id::TNat) (exp::TInt) (id'::TNat) (exp'::TInt) (u::Unit) :: Unit where
    InsertUnit LT id exp id' exp' us = '(id,exp) ': '(id',exp') ': us
    InsertUnit EQ id exp id' exp' us = '(id,Add exp exp') ': us
    InsertUnit GT id exp id' exp' ('(id'',exp'') ': us) = '(id',exp') ': InsertUnit (Cmp id id'') id exp id'' exp'' us
    InsertUnit GT id exp id' exp' '[] = ['(id',exp'),'(id,exp)]

type family (x::Unit) * (y::Unit) :: Unit where
    '[] * ys = ys
    ('(id,exp) ': xs) * ('(id',exp') ': ys) = xs * InsertUnit (Cmp id id') id exp id' exp' ys
infixl 7 *

type (a::Unit) / (b::Unit) = a * RecipUnit b
infixl 7 /

type FromID (x::TNat) = '(x,Plus One) ': '[]
--type family FromID (x::TNat) :: Unit where
--    FromID x = '(x,Plus One) ': '[]

newtype Tagged (u::Unit) a = Tagged a

type Meter = FromID One
type Second = FromID (Succ One)

meter :: Num a => Tagged Meter a
meter = Tagged 1

second :: Num a => Tagged Meter a
second = Tagged 1

gravity :: Num a => Tagged (Meter / (Second * Second)) a
gravity = undefined


