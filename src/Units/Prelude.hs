{-# LANGUAGE TypeOperators #-}

module Units.Prelude (
        module Prelude,
        module Units.Internals
    ) where

import Prelude hiding ( Num(negate, (+), (-), (*), abs, signum)
                      , Fractional((/), recip)
                      , Floating(sqrt) -- The other Floating functions are dimensionless
                      , subtract
--                      , (^), (^^) -- I'll hide these later, if I ever add exponents.
                      )
import Units.Internals hiding (TInt(..))

-- For the sake of simplicity, I've decided that all number-theory related / Integral
-- things are "pure", dimentionless math functions (e.g. gcd, fromIntegral, etc.),
-- so they're not shadowed by tagged counterparts.

