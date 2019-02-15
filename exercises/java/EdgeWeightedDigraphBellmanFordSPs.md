---
layout: exercise
title: Edge Weighted Digraph Bellman-Ford's Shortest Paths
question_text: >-
  Is there a directed path in a given edge-weighted digraph (with no negative cycles)
  from a source vertex <i>s</i> to a given target vertex <i>t</i>?
  If so, find a <i>shortest</i> such path (one whose total weight is minimal)
  in O(E*V) time and O(V) space.
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/tinyEWDn.png" title="tinyEWDn"></td>
      <td><img relative_src="/assets/images/tinyEWDnc.png" title="tinyEWDnc"></td>
    </tr>
  </table>
solution_file: /sources/java/BellmanFordSPs.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Bag.java
- /sources/java/DirectedEdge.java
- /sources/java/EdgeWeightedDigraph.java
- /sources/java/EdgeWeightedDigraphCycle.java
- /sources/java/GraphUtil.java
- /sources/java/Queue.java
- /sources/java/Stack.java
- /sources/java/Prime.java
language: java
command: javac BellmanFordSPs.java && java BellmanFordSPs
---
