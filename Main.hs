
{-# LANGUAGE TypeOperators #-}

import qualified Prelude as P
import Prelude hiding (Num(..),Fractional(..))
import UnitCalc

main :: IO ()
main = return ()

-- Some examples of uses

gravity :: Tagged (Meter / (Second * Second)) Double
gravity = 9.8 * meter / (second*second)

getWeight :: Tagged Kilogram Double -> Tagged Newton Double
getWeight x = x * gravity

-- Coulomb's Law
coulombsConstant :: Tagged (Newton * Meter * Meter / (Coulomb * Coulomb)) Double
coulombsConstant = Tagged 8.98755e9

forceBtwCharges :: Tagged Coulomb Double -> Tagged Coulomb Double -> Tagged Meter Double -> Tagged Newton Double
forceBtwCharges q1 q2 d = coulombsConstant * q1 * q2 / (d * d)

howHeigh :: Tagged (Meter/Second) Double -> Tagged Meter Double
howHeigh v = v*v / gravity

