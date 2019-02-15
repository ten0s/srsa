---
layout: exercise
title: Graph DFS Paths
question_text: >-
  Is there a path from <i>s</i> to a given target vertex <i>v</i>?
  If so, find such a path
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/tinyG.png" title="tinyG"></td>
    </tr>
  </table>
solution_file: /sources/java/GraphDFSPaths.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Graph.java
- /sources/java/GraphUtil.java
- /sources/java/DirectedEdge.java
- /sources/java/Bag.java
- /sources/java/Stack.java
language: java
command: javac GraphDFSPaths.java && java GraphDFSPaths
---
