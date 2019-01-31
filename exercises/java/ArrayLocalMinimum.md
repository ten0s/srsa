---
layout: exercise
title: Array Local Minimum
question_text: >-
  Given an array of n <b>distinct</b> integers, find a <i>strict local minimum</i>:
  an entry a[i] that is stricly less than its neighbors. Each internal entry
  (other than a[0] and a[n-1]) has 2 neighbors. There can be more than one local
   minima in an array, find any one of them. The program should run in O(log(n)).
solution_file: /sources/java/ArrayLocalMinimum.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
language: java
command: javac ArrayLocalMinimum.java && java ArrayLocalMinimum
---
