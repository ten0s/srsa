---
layout: exercise
title: Digraph DFS Single and Multiple Source Reachability
question_text: >-
  <ul>
    <li>Is there a directed path from <i>s</i> to a given target vertex <i>v</i>?</li>
    <li>Is there a directed path from <i>some</i> vertex in the set to a given target vertex <i>v</i>?</li>
  </ul>
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/tinyDG.png" title="tinyDG"></td>
    </tr>
  </table>
solution_file: /sources/java/DigraphDFS.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Digraph.java
- /sources/java/GraphUtil.java
- /sources/java/DirectedEdge.java
- /sources/java/Bag.java
language: java
command: javac DigraphDFS.java && java DigraphDFS
---
