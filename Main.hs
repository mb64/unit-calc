
{-# LANGUAGE TypeOperators #-}

import qualified Prelude as P
import Prelude hiding (Num(..),Fractional(..))
import UnitCalc

main :: IO ()
main = return ()

-- Some examples of uses

gravity :: P.Fractional a => Tagged (Meter / (Second * Second)) a
gravity = 9.8 * meter / (second*second)

getWeight :: P.Fractional a => Tagged Kilogram a -> Tagged Newton a
getWeight x = x * gravity

-- Coulomb's Law
coulombsConstant :: P.Fractional a => Tagged (Newton * Meter * Meter / (Coulomb * Coulomb)) a
coulombsConstant = Tagged 8.98755e9

forceBtwCharges :: P.Fractional a => Tagged Coulomb a -> Tagged Coulomb a -> Tagged Meter a -> Tagged Newton a
forceBtwCharges q1 q2 d = coulombsConstant * q1 * q2 / (d * d)

