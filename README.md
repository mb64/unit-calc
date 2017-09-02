# unit-calc
Tags numbers with unit types to enforce dimentional accuracy of calculations

It uses a plethora of GHC extentions, and surprisingly little code, to accomplish this goal at compile time.
All the work's in `src/Units/Internals.hs`, but there are a couple extremely simple examples in `src/Units/Examples.hs`.
It's probably most useful just for using GHCi as a calculator right now, but I might add more in the future.

A possible (future) setup to use the library:
 * GHC extensions `NoImplicitPrelude`, `TypeOperators`
 * `import qualified Prelude` if you really need to
 * Import `Units.Prelude` and your preferred unit package

Unit packages: It'll be something like `Units.SI`, `Units.Gaussian`, `Units.Common.{Metric,Imperial}` where each exports (hopefully
unique) identifiers for its units, as well as compound units.

I'm hoping for `Units.Common` to have simple commonly-used units like pounds, kilograms, feet, degrees Fahrenheit, and
non-scientific things like conversions between kilos and pounds. Possibly currency also, or maybe in a separate module.

A brief to-do list:
 * exponentiation -- I'll get to this eventually
 * Implement Units.SI, Units.Gaussian, etc. modules
 * generate stuff w/ TH:
    * Unit types (e.g. `Meter`), unit constants (e.g. `meter`) -- currently written out
    * Conversion units (e.g. `centimetersPerYard`, etc.) -- not there at the moment, probably more pressing than the first bullet

