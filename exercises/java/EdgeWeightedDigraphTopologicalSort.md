---
layout: exercise
title: Edge Weighted Digraph Topological Sort
question_text: >-
  Can a given edge-weighted digraph be topologicaly sorted? If so, find such an order
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/ewdigraph4.png" title="ewdigraph4"></td>
      <td><img relative_src="/assets/images/ewdigraph4-cycle.png" title="ewdigraph4-cycle"></td>
    </tr>
  </table>
solution_file: /sources/java/EdgeWeightedDigraphTopologicalSort.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/EdgeWeightedDigraph.java
- /sources/java/DirectedEdge.java
- /sources/java/GraphUtil.java
- /sources/java/Bag.java
- /sources/java/Queue.java
- /sources/java/Prime.java
- /sources/java/Stack.java
- /sources/java/EdgeWeightedDigraphCycle.java
- /sources/java/EdgeWeightedDigraphOrders.java
language: java
command: javac EdgeWeightedDigraphTopologicalSort.java && java EdgeWeightedDigraphTopologicalSort
---
