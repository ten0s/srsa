---
layout: exercise
title: Binary Search Tree Put
question_text: >-
  Put K/V into binary search tree
    <pre>
           +------(4,four)-----+
           |                   |
     +--(2,two)--+       +--(5,five)--+
     |           |       |            |
  (1,one)   (3,three)           +--(6,six)--+
                                |           |
                                    +--(7,seven)--+
                                    |             |
                                            +--(8,eight)--+
                                            |             |
                                                     +--(9,nine)--+
                                                     |            |
                                                            +--(10,ten)--+
                                                            |            |
   </pre>
solution_file: /sources/java/BSTMapPut.java
library_files:
- /sources/java/BSTMap.java
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
language: java
command: javac BSTMapPut.java && java BSTMapPut
---
