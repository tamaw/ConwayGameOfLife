# Conways Game of Life in Haskell

Conways Game of Life built in Haskell with a text and SDL version.

Produces some interesting results when benchmarking and changing options.

## Dependencies

- sdl2 - `stack install sdl2`

## Build & Run

`stack ghc -- Main.hs -O2 -threaded -rtsopts`

`./Main +RTS -s -N2 -H400M -A400M`

## Some results

```text
  15,759,518,280 bytes allocated in the heap
         784,592 bytes copied during GC
       7,779,656 bytes maximum residency (2 sample(s))
         703,160 bytes maximum slop
             822 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        17 colls,    17 par    0.001s   0.001s     0.0000s    0.0001s
  Gen  1         2 colls,     1 par    0.001s   0.001s     0.0004s    0.0006s

  Parallel GC work balance: 59.01% (serial 0%, perfect 100%)

  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N2)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.007s  (  0.007s elapsed)
  MUT     time   12.390s  ( 10.670s elapsed)
  GC      time    0.002s  (  0.001s elapsed)
  EXIT    time    0.001s  (  0.001s elapsed)
  Total   time   12.400s  ( 10.680s elapsed)

  Alloc rate    1,271,961,576 bytes per MUT second

  Productivity  99.9% of total user, 99.9% of total elapsed
```
