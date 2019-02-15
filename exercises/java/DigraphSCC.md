---
layout: exercise
title: Digraph Strongly Connected Components
question_text: >-
  Preprocess digraph to answer the queries in O(1) time:
  <ul>
  <li>Are two vertices <i>v</i> and <i>w</i> are <i>strongly</i> connected?</li>
  <li>How many <i>strong</i> components does the digraph have?</li>
  </ul>
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/tinyDG-scc.png" title="tinyDG"></td>
    </tr>
  </table>
solution_file: /sources/java/DigraphSCC.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Digraph.java
- /sources/java/GraphUtil.java
- /sources/java/DirectedEdge.java
- /sources/java/DigraphOrders.java
language: java
command: javac DigraphSCC.java && java DigraphSCC
---
