# cycle-bases

Welcome to the **cycle-bases** toolkit for the study of graph cycle bases. This project aims to calculate minimal cycle bases for a given graph. Originally initiated by Prof. Dr. Peter Stadler and Dr. Christian Höner zu Siederdissen, this project was developed as part of the course "Advanced Methods of Bioinformatics." The toolkit includes several algorithms and functions, including horton, edge-short-cycles, gauss, dijkstra, and de-pina. Graph operations within the toolkit are generalized over all finite fields (prime fields). For more in-depth information, please refer to the project's wiki.

**Authors**: Simon Johanning and Camill Kaipf

## Usage

To use the **cycle-bases** toolkit, follow these steps:

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/), a Haskell build tool.

2. Read the `test/Spec.hs` file, which includes the test execution details.

3. Run the tests using Stack:

   ```bash
   stack test
   ```

## Input Format

The toolkit assumes graphs in a specific format:

- Graphs are imported from an adjacency list.
- Each entry in the adjacency list contains a 4-tuple `(tail, head, edge, weight)`.
- The edge label (integer) corresponds to the unnested adjacency list, with enumeration starting from 1.
- All graphs are treated as directed graphs with unique edges.

Check the `test/` directory for example input files.

## Algorithms and Functions

The **cycle-bases** toolkit provides several algorithms and functions:

- **horton**: Implementation of the Horton's algorithm.
- **edge-short-cycles**: Algorithm for detecting short cycles based on edges.
- **gauss**: Gauss elimination algorithm for cycle detection.
- **dijkstra**: Dijkstra's algorithm for cycle detection.
- **de-pina**: De Pina's algorithm for finding fundamental cycles in planar graphs.

## Generalization

Graph operations within the toolkit are generalized over all finite fields, specifically prime fields.

## Documentation

For more detailed information about individual algorithms, usage examples, and further details, please refer to the [project's wiki](https://github.com/ckaipf/cycle-bases/wiki).
