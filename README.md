# unit-calc
Tags numbers with unit types to enforce dimentional accuracy of calculations

It uses a plethora of GHC extentions, and surprisingly little code, to accomplish this goal at compile time. All the work's in `src/Units/Internals.hs`, but there are a couple extremely simple examples in `src/Examples.hs`.
It's probably most useful just for using GHCi as a calculator right now, but I might add more in the future.

A brief to-do list:
 * Other Num things - almost there, just a little bit left
 * exponentiation
 * square roots

