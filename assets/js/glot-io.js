/*jslint
   browser: true,
   devel: true,
   nomen: true
*/
/*global $ */

"use strict";

var GlotIO = (function () {
    function run(req) {
        // run.glot.io doesn't support cors
        // workaround through https://cors-anywhere.herokuapp.com/
        var url =
            Storage.getCORSAnywhereUrl() +
            "https://run.glot.io/languages/" + req.language + "/" + req.version;
        return $.ajax({
            url: url,
            type: "POST",
            headers: {
                "Authorization": "Token " + Storage.getGlotIOToken(),
                "Content-Type": "application/json"
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
