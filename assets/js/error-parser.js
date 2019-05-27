/*jslint
   browser: true,
   devel: true,
   nomen: true,
   vars: true,
   unparam: true,
*/
/*global _ */

"use strict";

var ErrorParser = (function () {
    /*
       file = "endianness.c"
       stdout = ""
       stderr =
`
endianness.c:11:1: error: control reaches end of non-void function [-Werror=return-type]
 }
 ^
`
    */
    function cppParse(file, stdout, stderr) {
        var re = new RegExp(file + ":(\\d+):(\\d+): (error|warning): ([\\s\\S]*?\\^)", "g");
        var annotations = [];
        var match = re.exec(stderr);
        while (match) {
            //console.log(match);
            annotations.push({
                "row": match[1] - 1,
                "column": match[2] - 1,
                "text": match[4],
                "type": match[3]
            });
            match = re.exec(stderr);
        }
        return annotations;
    }


    /*
       file = "insertion_sort.erl"
       stdout = "insertion_sort.erl:7: spec for undefined function sort/1"
       stderr = ""
    */
    function erlangParse(file, stdout, stderr) {
        var re = new RegExp(file + ":(\\d+): (.*)", "g");
        var annotations = [];
        var match = re.exec(stdout);
        while (match) {
            //console.log(match);
            annotations.push({
                "row": match[1] - 1,
                "column": 0,
                "text": match[2],
                "type": "error"
            });
            match = re.exec(stdout);
        }
        return annotations;
    }

    /*
       file = "ArrayBinarySearch.java"
       stdout = ""
       stderr =
`
ArrayBinarySearch.java:3: error: missing return statement
    }
    ^
1 error
`
    */
    function javaParse(file, stdout, stderr) {
        var re = new RegExp(file + ":(\\d+): (error|warning): ([\\s\\S]*?\\^)", "g");
        var annotations = [];
        var match = re.exec(stderr);
        while (match) {
            //console.log(match);
            annotations.push({
                "row": match[1] - 1,
                "column": 0,
                "text": match[3],
                "type": match[2]
            });
            match = re.exec(stderr);
        }
        return annotations;
    }

    /*
       file = "list_comprehension.py"
       stdout = ""
       stderr =
`
File "list_comprehension.py", line 5
  def odds(min, max):
    ^
IndentationError: expected an indented block
`
    */
    function pythonParse(file, stdout, stderr) {
        var reText = new RegExp(".*Error: (.*)");
        var matchText = reText.exec(stderr);
        //console.log(matchText);
        if (matchText) {
            var reRows = new RegExp("File \"" + file + "\", line (\\d+)", "g");
            //console.log(reRows);
            var annotations = [];
            var matchRows = reRows.exec(stderr);
            while (matchRows) {
                //console.log(matchRows);
                annotations.push({
                    "row": matchRows[1] - 1,
                    "column": 0,
                    "text": matchText[1],
                    "type": "error"
                });
                matchRows = reRows.exec(stderr);
            }
            return annotations;
        }
        return [];
    }

    function parse(lang, file, stdout, stderr) {
        switch (lang) {
        case "c":
            return cppParse(file, stdout, stderr);
        case "c++":
            return cppParse(file, stdout, stderr);
        case "erlang":
            return erlangParse(file, stdout, stderr);
        case "java":
            return javaParse(file, stdout, stderr);
        case "python":
            return pythonParse(file, stdout, stderr);
        default:
            return [];
        }
    }

    return {
        parse: parse,
    };
}());
