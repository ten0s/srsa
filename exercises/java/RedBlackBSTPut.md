---
layout: exercise
title: Red-Black BST Put
question_text: >-
  Put K/V into Red-Black BST
    <pre>
           +------(4,four)-----+
           |                   |
     +--(2,two)--+       +--(6,six)--+
     |           |       |           |
  (1,one)   (3,three) (5,five)   (7,seven)
  </pre>
  <img relative_src="/assets/images/red-black-bst-ops.png">
solution_file: /sources/java/RedBlackBSTPut.java
library_files:
- /sources/java/BinTree.java
- /sources/java/Entry.java
- /sources/java/Node.java
- /sources/java/Pair.java
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
language: java
command: javac RedBlackBSTPut.java && java RedBlackBSTPut
---
