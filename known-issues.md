KNOWN ISSUES
============

* [BUG] when in the 1st horizontal pass a segment is collapsed to zero, in the 2nd pass it might change direction. This results in a potentially unbounded growth of that segment.
* [BUG] multiple monotony constraints might replace a separation constraint but do not induce a margin (we may be able to show that this cannot happen)
* [BUG] one separate run of the geo-nudging might put two segments in the same position that do not have each other in their `pathsBefore` sets. This can mean that we introduce unnecessary eyes (fixed for: knock-knee situations)
* [BUG] segments get pressed into obstacles by EndOfWorld nodes (fixed for full nudging)
* [POTENTIAL BUG] margins between segments of the same path might be desired
* [IMPROVEMENT] we should add pseudo constraint edges for port -- port and port -- begin/end of obstacle pairs.
* [IMPROVEMENT] close fixed segments should be joined before routing. Routed paths need to be separated (e.g. by adding an additional bend to one of them)

