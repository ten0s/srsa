/*jslint
   browser: true,
   devel: true,
   nomen: true,
   vars: true
*/
/*global _, $, ace */

"use strict";

function Editor(type, id, text, mode) {
    var _editor = null;
    var _ranges = null;

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

        _ranges = foldRanges(text);
        // Remove +BEGIN_FOLD and +END_FOLD
        text = text.replace(/\+BEGIN_FOLD|\+END_FOLD/gi, "");

        _editor = ace.edit(id);
        _editor.setValue(text, -1);
        _editor.session.setMode("ace/mode/" + mode);
        _editor.setTheme("ace/theme/monokai");
        _editor.setOptions({
            enableBasicAutocompletion: true,
            //enableLiveAutocompletion: true,
        });

        window.setTimeout(function () {
            _.each(_ranges, function (range) {
                _editor.session.foldAll(range[0], range[1], 0);
            });
        }, 250);
    }

    function getText() {
        return _editor.getValue();
    }

    function setFontSize(size) {
        _editor.setFontSize(size);
    }

    function clearAnnotations() {
        _editor.session.clearAnnotations();
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
        _editor.session.setAnnotations(annotations);
    }

    makeEditor(type, id, text, mode);

    return {
        getText: getText,
        setFontSize: setFontSize,
        clearAnnotations: clearAnnotations,
        setAnnotations: setAnnotations,
    };
}
