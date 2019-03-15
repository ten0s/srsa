---
layout: exercise
title: String Huffman Trie to Dump
question_text: >-
  Store a given Huffman trie to dump.<br>
  Traverse a trie in preorder: when visit an internal
  node write '0'; when visit a leaf write '1', followed by the leaf's character.<br>
  For example, toDump(trie) => "01a01b01r01c1d".
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/abracadabra-trie.png" title="abracadabra-trie"></td>
    </tr>
  </table>
solution_file: /sources/java/HuffmanTrieToDump.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Huffman.java
language: java
command: javac HuffmanTrieToDump.java && java HuffmanTrieToDump
---
