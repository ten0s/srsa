---
layout: exercise
title: Merge Lists
question_text: >-
  You have two sorted arrays, where each element is an interval.
  Now, merge the two array, overlapping intervals can be merged
  as a single one.
  <pre>
  I/P:
    List 1: [1,2], [3,9]
    List 2: [4,5], [8,10], [11,12]
  O/P:
    [1,2], [3,10], [11,12]
  </pre>
  <a href="https://www.careercup.com/question?id=5697271283318784">www.careercup.com</a>
solution_file: /sources/erlang/merge_lists.erl
library_files:
language: erlang
command: erlc merge_lists.erl && erl -noshell -eval "merge_lists:main([])."
---
