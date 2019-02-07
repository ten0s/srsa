---
layout: exercise
title: Digraph Topological Sort
question_text: >-
  Can a given digraph be topologicaly sorted? If so, find such an order
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/digraph4.png" title="digraph4"></td>
      <td><img relative_src="/assets/images/digraph4-cycle.png" title="digraph4-cycle"></td>
    </tr>
  </table>
solution_file: /sources/java/DigraphTopologicalSort.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Digraph.java
- /sources/java/GraphUtil.java
- /sources/java/DirectedEdge.java
- /sources/java/Bag.java
- /sources/java/HashSet.java
- /sources/java/Queue.java
- /sources/java/Prime.java
- /sources/java/Stack.java
- /sources/java/DigraphCycle.java
- /sources/java/DigraphOrders.java
language: java
command: javac DigraphTopologicalSort.java && java DigraphTopologicalSort
---
