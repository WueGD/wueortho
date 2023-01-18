KNOWN ISSUES
============

* [BUG] the implementation of path order is incorrect because pseudo directions are applied inconsistently
* [BUG] monotony constraints currently induce a margin
* [BUG] multiple monotony constraints might replace a separation constraint but do not induce a margin (we may be able to show that this cannot happen)
* [BUG] one separate run of the geo-nudging might put two segments in the same position that do not have each other in their `nodesBefore` sets. This can mean that we introduce unnecessary eyes
* [IMPROVEMENT] the ovg should use slightly enlarged versions of the obstacles
* [IMPROVEMENT] we should add pseudo constraint edges for port -- port and port -- begin/end of obstacle pairs.
