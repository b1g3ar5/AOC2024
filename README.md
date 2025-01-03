# AOC2024

Advent of Code 2024 solutions in Haskell

A couple of slow solutions...


### Installation

This is a stack project, so you can probably compile and run using:

    stack build

and:

    stack run -- AOC2024-exe


I would recommend using ghcup to install the required verions of stack, ghc, cabal, etc. I call it using:

    ghcup tui



### Profiling

stack build --profile

stack exec --profile -- AOC2024-exe +RTS -p

