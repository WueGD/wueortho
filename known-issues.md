KNOWN ISSUES
============

* [BUG] multiple monotony constraints might replace a separation constraint but do not induce a margin (we may be able to show that this cannot happen)
* [BUG] one separate run of the geo-nudging might put two segments in the same position that do not have each other in their `nodesBefore` sets. This can mean that we introduce unnecessary eyes (special case: knock-knee situations)
* [BUG] segments get pressed into obstacles by EndOfWorld nodes
* [IMPROVEMENT] we should add pseudo constraint edges for port -- port and port -- begin/end of obstacle pairs.
* [POTENTIAL BUG] margins between segments of the same path might be desired
* [IMPROVEMENT] routing graph edges should be in the middle of their region
* [IMPROVEMENT] close fixed segments should be joined before routing. Routed paths need to be separated (e.g. by adding an additional bend to one of them)

