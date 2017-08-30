# unit-calc
Tags numbers with unit types to enforce dimentional accuracy of calculations

It uses a plethora of GHC extentions, and surprisingly little code, to accomplish this goal at compile time. All the work's in `src/Units/Internals.hs`, but there are a couple extremely simple examples in `src/Examples.hs`.
It's probably most useful just for using GHCi as a calculator right now, but I might add more in the future.

A brief to-do list:
 * Other Num things - almost there, just a little bit left
 * exponentiation
 * **Square roots**
 * Add other units of mass
 * Maybe lb-force too?
 * generate stuff w/ TH:
    * Unit types (e.g. `Meter`), unit constants (e.g. `meter`) -- currently written out
    * Conversion units (e.g. `centimetersPerYard`, etc.) -- not there at the moment, probably more pressing than the first bullet
 * fix the `hiding (...)` lists to include the right things

