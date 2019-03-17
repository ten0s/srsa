/*jslint
   browser: true,
   devel: true,
   nomen: true,
   vars: true
*/
/*global _, $, Editor, Storage */

"use strict";

(function () {
    function init() {
        $("#settings-btn").click(function () {
            var glotIOToken = Storage.getGlotIOToken();
            var corsAnywhereUrl = Storage.getCORSAnywhereUrl();
            var fontSize = Storage.getEditorFontSize();
            var theme = Storage.getEditorTheme();
            var themes = Editor.getThemes();
            $("#settings-glot-io-token").val(glotIOToken);
            $("#settings-cors-anywhere-url").val(corsAnywhereUrl);
            $("#settings-editor-font-size").val(fontSize);
            $("#settings-editor-themes").empty();
            _.each(themes, function (t) {
                var o = $("<option>")
                    .val(t.theme)
                    .text(t.caption)
                    .attr("selected", t.theme === theme);
                $("#settings-editor-themes").append(o);
            });
            $("#settings-modal").modal("show");
        });
        $("#settings-save-btn").click(function () {
            var glotIOToken = $("#settings-glot-io-token").val();
            var corsAnywhereUrl = $("#settings-cors-anywhere-url").val();
            var fontSize = $("#settings-editor-font-size").val();
            var theme = $("#settings-editor-themes").val();
            Storage.setGlotIOToken(glotIOToken);
            Storage.setCORSAnywhereUrl(corsAnywhereUrl);
            Storage.setEditorFontSize(parseInt(fontSize, 10));
            Storage.setEditorTheme(theme);
            $("#settings-modal").modal("hide");
        });
    }
    init();
}());
