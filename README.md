# cycle-bases
Toolkit for the study of graph cycle bases.
Minimal cycle bases are calculated for a given graph.
The project goes back to Prof. Dr. Peter Stadler and Dr. Christian Höner zu Siederdissen and was part of the course [*Advanced methods of bioinformatics*](https://www.bioinf.uni-leipzig.de/teaching/currentClasses/class233.html).
Currently implemented algorithms and functions: horton, edge-short-cycles, gauss, dijkstra and de-pina.
Graph operations are generalized over all finite fields (prime fields).
For more detailed information consult the project's wiki.

Authors: Simon Johanning and Camill Kaipf

## Usage

+ install stack 
+ read `test/Spec.hs`, which includes the test execution

```bash
stack test
```

## Input format
- Graph is imported from given adjacency list
- Every entry contains 4-tuple (tail, head, edge, weight) 
- Edge label (integer) corresponds to the unnested adjacency list (enumeration is 1-based)
- All graphs are treated as directed graphs with unique edges
- See `test/` for examples
