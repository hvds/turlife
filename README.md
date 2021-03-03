Simulation of Turing machine-based "organisms" on a cellular automaton (CA)
substrate.

Initial implementation has Conway's game of life as the CA; the organisms
face in a specific direction, and the Turing machine-style rules (the
"genome") allow turning 90 degrees left (L) or right (R), or going straight
ahead (S) before always moving one cell in the direction they are facing.

Each organism has a certain amount of energy; it gains energy by replacing
a live cell in the CA with a blank cell, and loses energy if it replaces
a blank cell with a live cell. Any move that would send its energy negative
results in death.

There is a global "metabolic rate": each organism gets this many moves
before the CA is advanced by one tick.

If an organism exceeds a certain energy threshold, it has a given
probability at each tick to reproduce: it pays an energy cost to copy its
genome (based on the size the genome), and the remaining energy is shared
between the resulting two organisms, which initially sit on the same cell
but facing in opposite directions.

If two organisms attempt to move into the same cell, the clash is currently
resolved by killing both; however other options may be investigated, including
fight, flight, and sexual reproduction.

My immediate plans are to add a configuration screen, and to introduce
mutation into the genome-copying process.
