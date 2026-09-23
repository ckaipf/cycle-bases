# cycle-bases

A Haskell library for computing **minimum cycle bases** of weighted graphs. The linear algebra is generic over the coefficient field, so bases can be computed over any prime field GF(*p*) and, for de Pina's algorithm, over the rationals.

## Algorithms

| Module | Description |
| --- | --- |
| `Horton` | Horton's algorithm: builds a candidate set of cycles and selects linearly independent ones in order of increasing weight. |
| `DePina` | De Pina's algorithm: builds the basis incrementally, using auxiliary vectors that are kept orthogonal to the cycles found so far. |
| `EdgeShort` | Edge-short cycles, formed by an edge and the shortest paths from both of its endpoints to a common vertex; Horton's candidate set. |
| `FloydWarshall` | All-pairs shortest paths, computed algebraically after Fineman and Robinson (2011). |
| `Gauss` | Gaussian elimination for extracting a linearly independent subset of vectors. |
| `Graph` | Graph, edge, subgraph and cycle types, with conversion to and from incidence vectors. |
| `TH` | Template Haskell helpers that generate conversion functions for many prime fields at once. |

## Getting started

### Requirements

- [Stack](https://docs.haskellstack.org/). The project uses the `lts-24.60` snapshot (GHC 9.10); Stack installs the matching compiler automatically.
- Alternatively, open the repository in the included [dev container](.devcontainer/), which provides Stack, the Haskell language server extension for VS Code, and a persistent build cache.

### Build and test

```bash
stack build
stack test
```

The test suite runs de Pina's algorithm on `test/example3` over the first 40 prime fields and prints the basis found for each.

## Usage

Build a graph from an adjacency list and pass a function that maps integers into the field to compute over:

```haskell
{-# LANGUAGE DataKinds #-}
import qualified Data.Vector                 as V
import           Data.FiniteField.PrimeField (PrimeField)
import           DePina                      (dePina)
import           Graph
import           Horton                      (horton)

-- A square 0-1-3-2 with the chord 2 -> 1; edges are Edge tail head label weight.
g :: Graph
g = fromAdjacencyList $ V.fromList
  [ [Edge 0 1 1 1, Edge 0 2 2 1]
  , [Edge 1 3 3 1]
  , [Edge 2 3 4 1, Edge 2 1 5 1]
  ]

gf2 :: Integer -> PrimeField 2
gf2 = fromInteger

main :: IO ()
main = do
  print (dePina g gf2)  -- [([3,4,5]),([1,2,5])]
  print (horton g gf2)  -- [([-5,-2,1]),([-5,-3,4])]
```

Cycles are printed as the labels of their edges. In Horton's output, a negative label marks an edge traversed against its direction.

## Input format

Graphs are read from a plain-text adjacency list, one line per vertex. Each line holds a sequence of edges, each written as four integers:

```
tail head label weight
```

- Edge labels are integers, numbered from 1 in the order the edges appear in the file.
- Graphs are directed and every edge must be unique.

For example, `test/example1` describes a graph with seven unit-weight edges:

```
0 1 1 1
1 2 2 1 1 3 3 1
2 3 4 1
3 4 5 1 3 5 6 1
4 5 7 1
```

More examples are in [`test/`](test/).

## Documentation

See the [project wiki](https://github.com/ckaipf/cycle-bases/wiki) for background on the algorithms and further examples.

## Background

The project was started by Prof. Dr. Peter Stadler and Dr. Christian Höner zu Siederdissen and developed as part of the course *Advanced Methods of Bioinformatics*.

## Authors

Simon Johanning and Camill Kaipf
