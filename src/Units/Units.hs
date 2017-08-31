{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Units.Units where
-- The module with all the units in it.
-- I'll probably add export lists to these sometime.

import qualified Prelude as P
import Prelude hiding (Num(..), Fractional(..), Floating(sqrt)) -- etc. TODO update this later
import qualified GHC.TypeLits as TL
import Units.Internals

type FromID (x::TL.Nat) = '(x,Plus 1) ': '[]

type One = '[]

type Meter = FromID 0       ; meter = meters            ; meters = Tagged 1 :: P.Num a => Tagged Meter a
type Kilometer = FromID 1   ; kilometer = kilometers    ; kilometers = Tagged 1 :: P.Num a => Tagged Kilometer a
type Centimeter = FromID 2  ; centimeter = centimeters  ; centimeters = Tagged 1 :: P.Num a => Tagged Centimeter a
type Millimeter = FromID 3  ; millimeter = millimeters  ; millimeters = Tagged 1 :: P.Num a => Tagged Millimeter a
type Inch = FromID 4        ; inch = inches             ; inches = Tagged 1 :: P.Num a => Tagged Inch a
type Foot = FromID 5        ; foot = feet               ; feet = Tagged 1 :: P.Num a => Tagged Foot a
type Yard = FromID 6        ; yard = yards              ; yards = Tagged 1 :: P.Num a => Tagged Yard a
type Mile = FromID 7        ; mile = miles              ; miles = Tagged 1 :: P.Num a => Tagged Mile a

kilometersPerMeter  = 0.001*kilometer/meter                             :: P.Fractional a => Tagged (Kilometer / Meter) a
centimetersPerMeter = 100*centimeter/meter                              :: P.Fractional a => Tagged (Centimeter / Meter) a
millimetersPerMeter = 1000*millimeter/meter                             :: P.Fractional a => Tagged (Millimeter / Meter) a
inchesPerMeter      = centimetersPerMeter / (2.54 * centimeter / inch)  :: P.Fractional a => Tagged (Inch / Meter) a
feetPerMeter        = inchesPerMeter / (12 * inch / foot)               :: P.Fractional a => Tagged (Foot / Meter) a
yardsPerMeter       = feetPerMeter / (3 * foot / yard)                  :: P.Fractional a => Tagged (Yard / Meter) a
milesPerMeter       = feetPerMeter / (5280 * foot / mile)               :: P.Fractional a => Tagged (Mile / Meter) a

-- TODO: add other units of mass
type Kilogram = FromID 10   ; kilogram = kilograms; kilograms = Tagged 1 :: P.Num a => Tagged Kilogram a

type Second = FromID 20     ; second = seconds  ; seconds = Tagged 1 :: P.Num a => Tagged Second a
type Minute = FromID 21     ; minute = minutes  ; minutes = Tagged 1 :: P.Num a => Tagged Minute a
type Hour = FromID 22       ; hour = hours      ; hours = Tagged 1 :: P.Num a => Tagged Hour a

minutesPerSecond    = minute / (60*second)  :: P.Fractional a => Tagged (Minute / Second) a
hoursPerSecond      = hour / (1440*second)  :: P.Fractional a => Tagged (Hour / Second) a

type Coulomb = FromID 30        ; coulomb = Tagged 1 :: P.Num a => Tagged Coulomb a

type Hertz = One / Second                           ; hertz = Tagged 1 :: P.Num a => Tagged Hertz a
type Newton = Kilogram * Meter / (Second * Second)  ; newton = newtons  ; newtons = Tagged 1 :: P.Num a => Tagged Newton a
type Joule = Newton * Meter                         ; joule = joules    ; joules = Tagged 1 :: P.Num a => Tagged Joule a
type Watt = Joule / Second                          ; watt = watts      ; watts = Tagged 1 :: P.Num a => Tagged Watt a
type Volt = Joule / Coulomb                         ; volt = volts      ; volts = Tagged 1 :: P.Num a => Tagged Volt a
type Ampere = Coulomb / Second                      ; ampere = amperes  ; amperes = Tagged 1 :: P.Num a => Tagged Ampere a
type Amp = Ampere                                   ; amp = ampere      ; amps = ampere
type Ohm = Volt / Ampere                            ; ohm = ohms        ; ohms = Tagged 1 :: P.Num a => Tagged Ohm a

