
{-# LANGUAGE 
  DataKinds,
  PolyKinds,
  TypeInType,
  RankNTypes, 
  TypeFamilies,
  TypeOperators,
  UndecidableInstances,
  FlexibleInstances
 #-}

module UnitCalc where

import qualified Prelude as P
import Prelude (($), (.), undefined, Maybe(..), Ordering(..), Show(..))

-- Type-level integers
data TNat = Succ TNat | One deriving Show
data TInt = Plus TNat | Minus TNat deriving Show -- No zero bc it'll be removed from the list if it's zero

-- List of (id,exponent) pairs
type Unit = [(TNat,TInt)]

type family Cmp (x::TNat) (y::TNat) :: Ordering where
    Cmp One One = EQ
    Cmp One (Succ y) = LT
    Cmp (Succ x) One = GT
    Cmp (Succ x) (Succ y) = Cmp x y

type family Negate (x::TInt) :: TInt where
    Negate (Plus x) = Minus x
    Negate (Minus x) = Plus x

type family Add (x::TInt) (y::TInt) :: Maybe TInt where -- Nothing if it's zero
    Add (Minus x) (Plus y) = Add (Plus y) (Minus x)
    Add (Minus (Succ x)) (Minus y) = Add (Minus x) (Minus (Succ y))
    Add (Minus One) (Minus y) = Just (Minus (Succ y))
    Add (Plus (Succ x)) (Plus y) = Add (Plus x) (Plus (Succ y))
    Add (Plus One) (Plus y) = Just (Plus (Succ y))
    Add (Plus (Succ x)) (Minus (Succ y)) = Add (Plus x) (Minus y)
    Add (Plus One) (Minus (Succ y)) = Just (Minus y)
    Add (Plus (Succ x)) (Minus One) = Just (Plus x)
    Add (Plus One) (Minus One) = Nothing

type family RecipUnit (u::Unit) :: Unit where
    RecipUnit '[] = '[]
    RecipUnit ('(id,exp) ': us) = '(id,Negate exp) ': RecipUnit us

type family MaybeCons (id::TNat) (exp::Maybe TInt) (us::Unit) :: Unit where
    MaybeCons id (Just x) us = '(id,x) ': us
    MaybeCons id (Nothing) us = us

type family InsertUnit (cmp::Ordering) (id::TNat) (exp::TInt) (id'::TNat) (exp'::TInt) (u::Unit) :: Unit where
    InsertUnit LT id exp id' exp' us = '(id,exp) ': '(id',exp') ': us
    InsertUnit EQ id exp id' exp' us = MaybeCons id (Add exp exp') us
    InsertUnit GT id exp id' exp' ('(id'',exp'') ': us) = '(id',exp') ': InsertUnit (Cmp id id'') id exp id'' exp'' us
    InsertUnit GT id exp id' exp' '[] = ['(id',exp'),'(id,exp)]

type family (x::Unit) * (y::Unit) :: Unit where
    '[] * ys = ys
    ('(id,exp) ': xs) * ('(id',exp') ': ys) = xs * InsertUnit (Cmp id id') id exp id' exp' ys
    xs * '[] = xs
infixl 7 *

type (a::Unit) / (b::Unit) = a * RecipUnit b
infixl 7 /

type FromID (x::TNat) = '(x,Plus One) ': '[]
--type family FromID (x::TNat) :: Unit where
--    FromID x = '(x,Plus One) ': '[]

newtype Tagged (u::Unit) a = Tagged a deriving (Show, P.Eq)

-- Num instance for no units
instance (P.Num a,u ~ '[]) => P.Num (Tagged u a) where
    fromInteger = Tagged . P.fromInteger
    Tagged a + Tagged b = Tagged $ a P.+ b
    Tagged a - Tagged b = Tagged $ a P.- b
    Tagged a * Tagged b = Tagged $ a P.* b
    negate (Tagged x) = Tagged $ P.negate x
    abs (Tagged x) = Tagged $ P.negate x
    signum (Tagged x) = Tagged $ P.signum x

instance (P.Fractional a,u ~ '[]) => P.Fractional (Tagged u a) where
    fromRational = Tagged . P.fromRational
    (/) = undefined
    recip = undefined

-- Basic numerical operations
(+),(-) :: P.Num a => Tagged u a -> Tagged u a -> Tagged u a
{-# INLINE (+) #-}
Tagged x + Tagged y = Tagged $ x P.+ y
infixl 6 +
{-# INLINE (-) #-}
Tagged x - Tagged y = Tagged $ x P.- y
infixl 6 -

(*) :: P.Num a => Tagged u a -> Tagged u' a -> Tagged (u * u') a
{-# INLINE (*) #-}
Tagged x * Tagged y = Tagged $ x P.* y

negate :: P.Num a => Tagged u a -> Tagged u a
{-# INLINE negate #-}
negate (Tagged a) = Tagged $ P.negate a

(/) :: P.Fractional a => Tagged u a -> Tagged u' a -> Tagged (u / u') a
{-# INLINE (/) #-}
Tagged x / Tagged y = Tagged $ x P./ y


type Meter = FromID One
type Second = FromID (Succ One)
type Kilogram = FromID (Succ (Succ One))

type Newton = Kilogram * Meter / (Second * Second)
type Joule = Newton * Meter
type Watt = Joule / Second

meter :: P.Num a => Tagged Meter a          ; meter = Tagged 1
second :: P.Num a => Tagged Second a        ; second = Tagged 1
kilogram :: P.Num a => Tagged Kilogram a    ; kilogram = Tagged 1

newton :: P.Num a => Tagged Newton a        ; newton = Tagged 1
joule :: P.Num a => Tagged Joule a          ; joule = Tagged 1
watt :: P.Num a => Tagged Watt a            ; watt = Tagged 1

gravity :: P.Fractional a => Tagged (Meter / (Second * Second)) a
gravity = 9.8 * meter / (second*second)

getWeight :: P.Fractional a => Tagged Kilogram a -> Tagged Newton a
getWeight x = x * gravity

