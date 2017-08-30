
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module UnitCalc where

import qualified Prelude as P
import Prelude hiding ( Num(..) -- Not hiding all the ones that have "dimentionless" math functions only
--                      , Real(..)
--                      , Integral(..)
                      , Fractional(..)
--                      , Floating(..)
--                      , RealFrac(..)
--                      , RealFloat(..)
                      , subtract
--                      , even, odd
--                      , gcd, lcm
                      , (^), (^^)
--                      , fromIntegral
--                      , realToFrac
                      )
import qualified GHC.TypeLits as TL

-- Type-level integers
-- Plus n means n+1, Minus n means -n-1 (so as not to include zero)
data TInt = Plus TL.Nat | Minus TL.Nat -- No zero bc it'll be removed from the list if it's zero

-- List of (id,exponent) pairs
type Unit = [(TL.Nat,TInt)]

type family Negate (x::TInt) :: TInt where
    Negate (Plus x) = Minus x
    Negate (Minus x) = Plus x

type family Sub (o::Ordering) (x::TL.Nat) (y::TL.Nat) :: Maybe TInt where
    Sub LT x y = Just (Minus (y TL.- x TL.- 1))
    Sub EQ x y = Nothing
    Sub GT x y = Just (Plus (x TL.- y TL.- 1))

type family Add (x::TInt) (y::TInt) :: Maybe TInt where -- Nothing if it's zero
    Add (Minus x) (Plus y) = Sub (TL.CmpNat y x) y x
    Add (Minus x) (Minus y) = Just (Minus (x TL.+ y TL.+ 1))
    Add (Plus x) (Plus y) = Just (Plus (x TL.+ y TL.+ 1))
    Add (Plus x) (Minus y) = Sub (TL.CmpNat x y) x y

type family RecipUnit (u::Unit) :: Unit where
    RecipUnit '[] = '[]
    RecipUnit ('(id,exp) ': us) = '(id,Negate exp) ': RecipUnit us

type family MaybeCons (id::TL.Nat) (exp::Maybe TInt) (us::Unit) :: Unit where
    MaybeCons id (Just x) us = '(id,x) ': us
    MaybeCons id (Nothing) us = us

type family InsertUnit (cmp::Ordering) (id::TL.Nat) (exp::TInt) (id'::TL.Nat) (exp'::TInt) (u::Unit) :: Unit where
    InsertUnit LT id exp id' exp' us = '(id,exp) ': '(id',exp') ': us
    InsertUnit EQ id exp id' exp' us = MaybeCons id (Add exp exp') us
    InsertUnit GT id exp id' exp' ('(id'',exp'') ': us) = '(id',exp') ': InsertUnit (TL.CmpNat id id'') id exp id'' exp'' us
    InsertUnit GT id exp id' exp' '[] = ['(id',exp'),'(id,exp)]

type family (x::Unit) * (y::Unit) :: Unit where
    '[] * ys = ys
    ('(id,exp) ': xs) * ('(id',exp') ': ys) = xs * InsertUnit (TL.CmpNat id id') id exp id' exp' ys
    xs * '[] = xs
infixl 7 *

type (a::Unit) / (b::Unit) = a * RecipUnit b
infixl 7 /

-- newtype for tagged numbers
newtype Tagged (u::Unit) a = Tagged a deriving (Show,Eq,Ord)
-- It's OK that the unit has a phantom role so you can coerce it to whatever units.

-- Num instance for no units
deriving instance (P.Num a,u ~ '[]) => P.Num (Tagged u a)
deriving instance (P.Real a,u ~ '[]) => P.Real (Tagged u a)
deriving instance (P.Enum a,u ~ '[]) => P.Enum (Tagged u a) -- For Integral instance
deriving instance (P.Integral a,u ~ '[]) => P.Integral (Tagged u a)
deriving instance (P.Fractional a,u ~ '[]) => P.Fractional (Tagged u a)
deriving instance (P.Floating a,u ~ '[]) => P.Floating (Tagged u a)
deriving instance (P.RealFrac a,u ~ '[]) => P.RealFrac (Tagged u a)
deriving instance (P.RealFloat a,u ~ '[]) => P.RealFloat (Tagged u a)

-- Basic numerical operations
(+),(-),subtract :: P.Num a => Tagged u a -> Tagged u a -> Tagged u a
Tagged x + Tagged y = Tagged $ x P.+ y
infixl 6 +
Tagged x - Tagged y = Tagged $ x P.- y
infixl 6 -
subtract (Tagged subtrahend) = \(Tagged minuend) -> Tagged $ minuend P.- subtrahend

negate :: P.Num a => Tagged u a -> Tagged u a
negate (Tagged a) = Tagged $ P.negate a

-- * and / inherit the fixity of the like-named type operators
(*) :: P.Num a => Tagged u a -> Tagged u' a -> Tagged (u * u') a
Tagged x * Tagged y = Tagged $ x P.* y

(/) :: P.Fractional a => Tagged u a -> Tagged u' a -> Tagged (u / u') a
Tagged x / Tagged y = Tagged $ x P./ y

{-# INLINE (+) #-}
{-# INLINE (-) #-}
{-# INLINE subtract #-}
{-# INLINE (*) #-}
{-# INLINE (/) #-}
{-# INLINE negate #-}

-- Units
type FromID (x::TL.Nat) = '(x,Plus 1) ': '[]

type One = '[]
type Meter = FromID 1
type Second = FromID 2
type Kilogram = FromID 3
type Coulomb = FromID 4

type Hertz = One / Second
type Newton = Kilogram * Meter / (Second * Second)
type Joule = Newton * Meter
type Watt = Joule / Second
type Volt = Joule / Coulomb
type Ampere = Coulomb / Second
type Ohm = Volt / Ampere

meter :: P.Num a => Tagged Meter a          ; meter = Tagged 1
second :: P.Num a => Tagged Second a        ; second = Tagged 1
kilogram :: P.Num a => Tagged Kilogram a    ; kilogram = Tagged 1
coulomb :: P.Num a => Tagged Coulomb a      ; coulomb = Tagged 1

hertz :: P.Num a => Tagged Hertz a          ; hertz = Tagged 1
newton :: P.Num a => Tagged Newton a        ; newton = Tagged 1
joule :: P.Num a => Tagged Joule a          ; joule = Tagged 1
watt :: P.Num a => Tagged Watt a            ; watt = Tagged 1
volt :: P.Num a => Tagged Volt a            ; volt = Tagged 1
ampere :: P.Num a => Tagged Ampere a        ; ampere = Tagged 1
ohm :: P.Num a => Tagged Ohm a              ; ohm = Tagged 1

