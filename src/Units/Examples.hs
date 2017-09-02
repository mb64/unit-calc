
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Units.Examples where

import qualified Prelude as P
import Units.Prelude
import Units.Units

-- Some examples of uses

gravity :: P.Fractional a => Tagged (Meter / (Second * Second)) a
gravity = (9.8 * meter) / (second*second)

getWeight :: P.Fractional a => Tagged Kilogram a -> Tagged Newton a
getWeight x = x * gravity

-- Coulomb's Law
coulombsConstant :: P.Fractional a => Tagged (Newton * Meter * Meter / (Coulomb * Coulomb)) a
coulombsConstant = Tagged 8.98755e9

forceBtwCharges :: P.Fractional a => Tagged Coulomb a -> Tagged Coulomb a -> Tagged Meter a -> Tagged Newton a
forceBtwCharges q1 q2 d = coulombsConstant * q1 * q2 / (d * d)

-- Another thing
howHigh :: P.Fractional a => Tagged (Meter/Second) a -> Tagged Meter a
howHigh v = v*v / gravity

howHighImperial :: forall a. P.Fractional a => Tagged (Mile / Hour) a -> Tagged Foot a
howHighImperial v = feetPerMeter * howHigh v'
  where v' :: Tagged (Meter / Second) a
        v' = v * hoursPerSecond / milesPerMeter

-- Pythagorean theorem (to demonstrate sqrt)
pythagoreanTheorem :: P.Floating a => Tagged Meter a -> Tagged Meter a -> Tagged Meter a
pythagoreanTheorem a b = sqrt $ a*a + b*b

