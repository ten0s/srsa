---
layout: exercise
title: Binary Search Tree Rank/Selection
question_text: >-
  Find <i>rank</i> (the number of keys less than a given key) and
  <i>selection</i> (the key with a given rank) in
  the binary search tree
  <pre>
        +----5----+
        |         |
        1--+   +--7--+
           |   |     |
           3   6     9
  </pre>
solution_file: /sources/java/BSTMapRankSelect.java
library_files:
- /sources/java/BSTMap.java
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
language: java
command: javac BSTMapRankSelect.java && java BSTMapRankSelect
---
