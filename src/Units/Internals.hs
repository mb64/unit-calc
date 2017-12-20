
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Units.Internals (
        -- Types
        Tagged(..),
        Unit,
        TInt(..),
        One,
        FromID,
        type (*),
        type (/),
        type (^),
        RecipUnit,
        Sqrt,
        -- Functions
        unTag,
        (+),(-),subtract,
        negate,
        (*),(/),
        recip,
        abs,signum,
        sqrt,
        pow
    ) where

import qualified Prelude as P
import Prelude hiding ( Num((+),(-),(*),negate,abs,signum) -- This should be the same import as in Units.Prelude
                      , Fractional((/),recip)
                      , Floating(sqrt)
                      , subtract
--                      , (^), (^^)
                      )
import qualified GHC.TypeLits as TL
import Data.Proxy (Proxy(..))

-- Type-level integers
-- Plus n means n+1, Minus n means -n-1 (so as not to include zero)
data TInt = Plus TL.Nat | Minus TL.Nat -- No zero bc it'll be removed from the list if it's zero

-- List of (id,exponent) pairs
type Unit = [(TL.Nat,TInt)]

-- A few useful types
type FromID (x::TL.Nat) = '(x,Plus 0) ': '[]
type One = '[]

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

type family Halve (x::TL.Nat) :: TL.Nat where
    Halve 0 = TL.TypeError (TL.Text "Fractional exponents in units are not allowed.")
    Halve 1 = 0 -- Unintuitive arithmatic because (Plus or Minus) x represents ±(x+1)
    Halve x = 1 TL.+ Halve (x TL.- 2)

type family Sqrt (u::Unit) :: Unit where
    Sqrt ('(id,Plus exp) ': us) = '(id,Plus (Halve exp)) ': Sqrt us
    Sqrt ('(id,Minus exp) ': us) = '(id,Minus (Halve exp)) ': Sqrt us
    Sqrt '[] = '[]

type family (u::Unit) ^ (e::TL.Nat) :: Unit where
    u ^ 0 = One
    u ^ n = u * u^(n TL.- 1)
infixr 8 ^

-- newtype for tagged numbers; not Data.Tagged because of the explicit Unit kind.
newtype Tagged (u::Unit) a = Tagged a deriving (Show,Eq,Ord)
-- It's OK that the unit has a phantom role so you can coerce it to whatever units if you want.

unTag :: Tagged u a -> a
unTag (Tagged x) = x

-- Num instance for no units, using GND
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

recip :: P.Fractional a => Tagged u a -> Tagged (RecipUnit u) a
recip (Tagged x) = Tagged $ P.recip x

abs :: P.Num a => Tagged u a -> Tagged u a
abs (Tagged x) = Tagged $ P.abs x

signum :: P.Num a => Tagged u a -> Tagged u a
signum (Tagged x) = Tagged $ P.signum x

sqrt :: P.Floating a => Tagged u a -> Tagged (Sqrt u) a
sqrt (Tagged x) = Tagged $ P.sqrt x

-- Use with type application, like pow @2 x
pow :: forall e u a. (TL.KnownNat e,P.Num a) => Tagged u a -> Tagged (u ^ e) a
pow (Tagged x) = Tagged $ x P.^ e :: Tagged (u ^ e) a
  where e :: Integer
        e = TL.natVal (Proxy :: Proxy e)

{-# INLINE (+) #-}
{-# INLINE (-) #-}
{-# INLINE subtract #-}
{-# INLINE (*) #-}
{-# INLINE (/) #-}
{-# INLINE recip #-}
{-# INLINE negate #-}
{-# INLINE abs #-}
{-# INLINE signum #-}
{-# INLINE sqrt #-}
{-# INLINE pow #-}

