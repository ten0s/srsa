/*jslint
   browser: true,
   devel: true,
   nomen: true,
   vars: true
*/
/*global _, $, ace */

"use strict";

function fold_ranges(text) {
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

function make_editor(type, id, text, mode) {
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

    var ranges = fold_ranges(text);
    // Remove +BEGIN_FOLD and +END_FOLD
    text = text.replace(/\+BEGIN_FOLD|\+END_FOLD/gi, "");

    var editor = ace.edit(id);
    editor.setValue(text, -1);
    editor.setTheme("ace/theme/monokai");
    editor.setFontSize(15);
    editor.session.setMode("ace/mode/" + mode);

    window.setTimeout(function () {
        _.each(ranges, function (range) {
            editor.session.foldAll(range[0], range[1], 0);
        });
    }, 300);

    return editor;
}
