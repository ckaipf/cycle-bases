# cycle-bases
Small toolkit for the study of graph cycle bases.
Minimal cycle bases are calculated for an input graph.
The idea goes back to Prof. Dr. Peter Stadler and Dr. Christian Höner zu Siederdissen and was part of the course [*Advanced methods of bioinformatics*](https://www.bioinf.uni-leipzig.de/teaching/currentClasses/class233.html).
Currently implemented algorithms and functions: horton, edge-short-cycles, gauss, dijkstra and de-pina
Graph arithmetic is generalized over all finite fields.

Authors: Simon Johanning and Camill Kaipf

## Usage
+ install stack 
+ read test/Spec.hs which includes the test execution

```bash
stack test
```

## Input format
- Graph is imported from adjacency list
- Every entry contains 4-tuple (tail, head, edge, weight) 
- Edge (integer) label corresponds to unnested adjacency list (enumeration 1-based)
- All graphs are treated as directed graphs with unique edges
- See test/ for examples
