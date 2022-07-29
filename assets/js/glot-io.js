/*jslint
   browser: true,
   devel: true,
   nomen: true
*/
/*global $ */

"use strict";

// https://github.com/glotcode/glot/tree/master/api_docs/run

var GlotIO = (function () {
    function run(req) {
        // glot.io doesn't support cors
        // workaround using https://github.com/Rob--W/cors-anywhere/
        var url =
            Storage.getCORSAnywhereUrl() +
            "https://glot.io/api/run/" + req.language + "/" + req.version;
        return $.ajax({
            url: url,
            type: "POST",
            headers: {
                "Authorization": "Token " + Storage.getGlotIOToken(),
                "Content-Type": "application/json",
            },
            data: JSON.stringify({
                files: req.files,
                command: req.command
            })
        });
    }

    return {
        run: run
    };
}());
