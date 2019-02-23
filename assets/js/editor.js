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

        _editor = ace.edit(id);
        _editor.setValue(text, -1);
        _editor.session.setMode("ace/mode/" + mode);
        _editor.setOptions({
            enableBasicAutocompletion: true,
            //enableLiveAutocompletion: true,
        });

        $(document).ready(function () {
            window.setTimeout(function () {
                _.each(ranges, function (range) {
                    _editor.session.foldAll(range[0], range[1], 0);
                });
            }, 100);
        });
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

    function setAnnotations(annotations) {
        // if some annotations are folded, add them to the fold's top
        var folds = _editor.session.getAllFolds();
        //console.log(folds);
        _.each(annotations, function (a) {
            _.each(folds, function (f) {
                if (f.start.row <= a.row && a.row <= f.end.row) {
                    annotations.push({
                        "row": f.start.row, // zero-based
                        "column": a.column, // zero-based
                        "text": a.text,     // tooltip text
                        "type": a.type      // "error" | "warning" | "info"
                    });
                }
            });
        });
        _editor.session.setAnnotations(annotations);
    }

    function setTheme(theme) {
        _editor.setTheme(theme);
    }

    makeEditor(type, id, text, mode);

    return {
        getText: getText,
        setFontSize: setFontSize,
        setTheme: setTheme,
        clearAnnotations: clearAnnotations,
        setAnnotations: setAnnotations,
    };
}

Editor.getThemes = function () {
    var themelist = ace.require("ace/ext/themelist");
    return themelist.themes;
};
