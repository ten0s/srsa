/*jslint
   browser: true,
   devel: true,
   nomen: true,
   vars: true
*/
/*global _, $, ace */

"use strict";

function Editor(type, id, text, mode) {
    var editor = null;

    function foldRanges(text) {
        var lines = text.split(/\r\n|\r|\n/);
        var ranges = [], begin = -1;
        _.each(lines, function (line, number) {
            if (/\+BEGIN_FOLD/.exec(line)) {
                //console.log("beg: " + number);
                begin = number;
            }
            if (/\+END_FOLD/.exec(line)) {
                //console.log("end: " + number);
                ranges.push([begin, number]);
            }
        });
        //console.log(ranges);
        return ranges;
    }

    function makeEditor(type, id, text, mode) {
        switch (type) {
        case "question":
            // Remove +BEGIN_SOLUTION, +END_SOLUTION and everything in between
            text = text.replace(/\s*[\/%#]+\+BEGIN_SOLUTION[\s\S]*?[\/%#]+\+END_SOLUTION/gi, "");
            break;
        case "solution":
            // Remove +BEGIN_SOLUTION and +END_SOLUTION
            text = text.replace(/\s*([\/%#]+\+BEGIN_SOLUTION|[\/%#]+\+END_SOLUTION)/gi, "");
            break;
        default:
            throw new Error("Unknown editor type: " + type);
        }

        // Remove +BEGIN_REMOVE, +END_REMOVE and everything in between
        text = text.replace(/\s*[\/%#]+\+BEGIN_REMOVE[\s\S]*?[\/%#]+\+END_REMOVE/gi, "");

        var ranges = foldRanges(text);
        // Remove +BEGIN_FOLD and +END_FOLD
        text = text.replace(/\+BEGIN_FOLD|\+END_FOLD/gi, "");

        editor = ace.edit(id);
        editor.setValue(text, -1);
        editor.session.setMode("ace/mode/" + mode);
        editor.setTheme("ace/theme/monokai");
        editor.setOptions({
            enableBasicAutocompletion: true,
            //enableLiveAutocompletion: true,
        });

        window.setTimeout(function () {
            _.each(ranges, function (range) {
                editor.session.foldAll(range[0], range[1], 0);
            });
        }, 300);
    }

    function getText() {
        return editor.getValue();
    }

    function setFontSize(size) {
        editor.setFontSize(size);
    }

    function clearAnnotations() {
        editor.session.clearAnnotations();
    }

    /*
      var annotations = [{
          row: 1,                      // zero-based
          column: 0,                   // zero-based
          text: "Strange error",       // tooltip text
          type: "error"                // "error" | "warning" | "info"
      }];
    */
    function setAnnotations(annotations) {
        editor.session.setAnnotations(annotations);
    }

    makeEditor(type, id, text, mode);

    return {
        getText: getText,
        setFontSize: setFontSize,
        clearAnnotations: clearAnnotations,
        setAnnotations: setAnnotations,
    };
}
