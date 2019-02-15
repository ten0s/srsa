---
layout: exercise
title: Edge Weighted Digraph Dijkstra's Shortest Path
question_text: >-
  Is there a directed path in a given edge-weighted digraph (with nonnegative edges)
  from a source vertex <i>s</i> to a target vertex <i>t</i>?
  If so, find a <i>shortest</i> such path (one whose total weight is minimal).
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/tinyEWG-directed.png" title="tinyEWG-directed"></td>
    </tr>
  </table>
solution_file: /sources/java/DijkstraSP.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Bag.java
- /sources/java/DirectedEdge.java
- /sources/java/EdgeWeightedDigraph.java
- /sources/java/GraphUtil.java
- /sources/java/Queue.java
- /sources/java/Stack.java
- /sources/java/Prime.java
- /sources/java/IndexMinPQ.java
- /sources/java/IndexPQ.java
language: java
command: javac DijkstraSP.java && java DijkstraSP
---
