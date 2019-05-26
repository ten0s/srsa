---
layout: exercise
title: String Search Boyer-Moore
question_text: >-
  Implement Boyer-Moore substring search
  <table border="1">
    <tr>
      <td><img relative_src="/assets/images/boyer-moore.png" title="boyer-moore"></td>
      <td>
        <pre>
   right
   ------
   . | -1
   E |  5
   D |  3
   . | -1
   L |  4
   N |  0
   . | -1
        </pre>
      </td>
    </tr>
  </table>
solution_file: /sources/java/StringSearchBoyerMoore.java
library_files:
- /sources/java/Assert.java
- /sources/java/ArrayUtil.java
language: java
command: javac StringSearchBoyerMoore.java && java StringSearchBoyerMoore
---
