Graph Drawing with Simple Algorithms
====================================

What we want:
-------------

* Simple Algorithms (hardest: LP, the easier the better)
* Separate, independent pipeline stages
* Support for
    - orthogonal edges
    - terminals
    - fixed node positions

Ideas:
------

* use the layered layout of iPraline and only apply the edge routing (or even only the nudging) step
* reroute edges if their bundle is too dense
