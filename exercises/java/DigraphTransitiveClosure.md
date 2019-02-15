---
layout: exercise
title: Digraph Transitive Closure
question_text: >-
  Is there a directed path from a given vertex <i>v</i> to another given vertex <i>w</i>?
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/tinyDG.png" title="tinyDG"></td>
    </tr>
  </table>
solution_file: /sources/java/DigraphTransitiveClosure.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Digraph.java
- /sources/java/DigraphDFS.java
- /sources/java/GraphUtil.java
- /sources/java/DirectedEdge.java
- /sources/java/Bag.java
language: java
command: javac DigraphTransitiveClosure.java && java DigraphTransitiveClosure
---
