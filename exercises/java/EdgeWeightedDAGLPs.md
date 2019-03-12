---
layout: exercise
title: Edge Weighted DAG Longest Paths
question_text: >-
  Is there a directed path in a given edge-weighted DAG (with negative edges allowed)
  from a source vertex <i>s</i> to a given target vertex <i>t</i>?
  If so, find a <i>longest</i> such path (one whose total weight is maximal)
  in O(E+V) time and O(V) space.
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/tinyEWDAG.png" title="tinyEWDAG"></td>
    </tr>
  </table>
solution_file: /sources/java/EdgeWeightedDAGLPs.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Either.java
- /sources/java/DirectedEdge.java
- /sources/java/EdgeWeightedDigraph.java
- /sources/java/GraphUtil.java
- /sources/java/EdgeWeightedDigraphTopologicalSort.java
- /sources/java/EdgeWeightedDigraphCycle.java
- /sources/java/EdgeWeightedDigraphOrders.java
- /sources/java/EdgeWeightedDAGSPs.java
language: java
command: javac EdgeWeightedDAGLPs.java && java EdgeWeightedDAGLPs
---
