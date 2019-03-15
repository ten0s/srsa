---
layout: exercise
title: String Huffman Trie from Dump
question_text: >-
  Recover Huffman trie from a given dump.<br>
  Read a single character to learn which type of node comes next:
  if it's '1' then read the next character and create a leaf; if it's '0' then create an internal node and recursively build its left and right subtress.<br>
  For example, trieFromDump("01a01b01r01c1d") should produce the trie below.
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/abracadabra-trie.png" title="abracadabra-trie"></td>
    </tr>
  </table>
solution_file: /sources/java/HuffmanTrieFromDump.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
- /sources/java/Huffman.java
language: java
command: javac HuffmanTrieFromDump.java && java HuffmanTrieFromDump
---
