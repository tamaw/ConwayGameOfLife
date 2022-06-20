# Conways Game of Life in Haskell

Conways Game of Life built in Haskell with a text and SDL version which uses threads.

Produces some interesting results when benchmarking and changing options.

## Dependencies

- sdl2 - `stack install sdl2`

## Screenshots

![image](https://user-images.githubusercontent.com/461535/174538486-ae88a841-3182-4647-b27d-ee3eb176826e.png)
![image](https://user-images.githubusercontent.com/461535/174538801-f301d72c-3228-4f5d-b100-03d400684ec4.png)


## Some results

This seems to be the sanest best performance options on my laptop.

`stack ghc -- Main.hs -O2 -threaded -rtsopts && ./Main +RTS -s -N2 -H400M -A400M`

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

Much slower without optimisation flag

`stack ghc -- Main.hs -threaded -rtsopts && ./Main +RTS -s`

```text
 620,466,883,536 bytes allocated in the heap
  18,399,970,392 bytes copied during GC
       7,909,752 bytes maximum residency (2116 sample(s))
         712,168 bytes maximum slop
              18 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     600579 colls,     0 par   13.470s  13.831s     0.0000s    0.0007s
  Gen  1      2116 colls,     0 par    0.366s   0.368s     0.0002s    0.0006s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.002s  (  0.002s elapsed)
  MUT     time  144.682s  (137.128s elapsed)
  GC      time   13.836s  ( 14.199s elapsed)
  EXIT    time    0.000s  (  0.003s elapsed)
  Total   time  158.520s  (151.331s elapsed)

  Alloc rate    4,288,493,196 bytes per MUT second

  Productivity  91.3% of total user, 90.6% of total elapsed
```
Going to -O3 improves performance but only by a fraction.

`stack ghcstack ghc -- Main.hs -O3 -threaded -rtsopts && ./Main +RTS -s -N2`

```text
  15,775,019,824 bytes allocated in the heap
      10,750,072 bytes copied during GC
       7,704,584 bytes maximum residency (2 sample(s))
         692,216 bytes maximum slop
              11 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      9010 colls,  9010 par    0.093s   0.050s     0.0000s    0.0004s
  Gen  1         2 colls,     1 par    0.001s   0.000s     0.0002s    0.0002s

  Parallel GC work balance: 4.82% (serial 0%, perfect 100%)

  TASKS: 6 (1 bound, 5 peak workers (5 total), using -N2)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time   11.820s  ( 10.218s elapsed)
  GC      time    0.093s  (  0.050s elapsed)
  EXIT    time    0.001s  (  0.002s elapsed)
  Total   time   11.915s  ( 10.270s elapsed)

  Alloc rate    1,334,552,950 bytes per MUT second

  Productivity  99.2% of total user, 99.5% of total elapsed
```

Increasing the threads reduces performance which seems to slow the garbage collector with each additional thread.

`stack ghc -- Main.hs -O3 -threaded -rtsopts && ./Main +RTS -s -N6`

```text
  15,775,082,240 bytes allocated in the heap
      20,443,744 bytes copied during GC
       7,752,088 bytes maximum residency (2 sample(s))
         747,112 bytes maximum slop
              15 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      9118 colls,  9118 par    1.481s   0.077s     0.0000s    0.0006s
  Gen  1         2 colls,     1 par    0.001s   0.000s     0.0002s    0.0003s

  Parallel GC work balance: 1.42% (serial 0%, perfect 100%)

  TASKS: 28 (1 bound, 13 peak workers (27 total), using -N6)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.003s  (  0.001s elapsed)
  MUT     time   14.149s  ( 11.165s elapsed)
  GC      time    1.482s  (  0.078s elapsed)
  EXIT    time    0.003s  (  0.007s elapsed)
  Total   time   15.637s  ( 11.250s elapsed)

  Alloc rate    1,114,930,875 bytes per MUT second

  Productivity  90.5% of total user, 99.2% of total elapsed
```

