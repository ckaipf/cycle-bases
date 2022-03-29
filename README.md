# cycle-bases
Currently implemented algorithms and functions: horton, edge-short-cycles, gauss, dijkstra and de-pina

Project work in the course 'Advanced methods of bioinformatics' ([bioinf home page](https://www.bioinf.uni-leipzig.de/teaching/currentClasses/class233.html))

## usage
+ install stack 
+ initialize repository 
+ read test/Spec.hs which includes the test execution

```bash
stack test
```

## input format
Graph is imported from adjacency list. Every entry contains 4-tuple (tail,head,edge,weight). Edge (integer) label corresponds to unnested adjacency list (enumeration 1-based). All graphs are treated as directed graphs with unique edges. See test/ for examples.
