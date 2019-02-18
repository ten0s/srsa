/*jslint
   browser: true,
   devel: true,
   nomen: true,
   vars: true
*/
/*global _, $, ace */

"use strict";

(function () {
    function initSettingsBtn() {
        $("#settings-btn").click(function () {
            var glotIOToken = Storage.getGlotIOToken();
            var fontSize = Storage.getEditorFontSize();
            $("#settings-glot-io-token").val(glotIOToken);
            $("#settings-editor-font-size").val(fontSize);
            $("#settings-modal").modal("show");
        });
        $("#settings-save-btn").click(function () {
            var glotIOToken = $("#settings-glot-io-token").val();
            var fontSize = $("#settings-editor-font-size").val();
            Storage.setGlotIOToken(glotIOToken);
            Storage.setEditorFontSize(parseInt(fontSize, 10));
            $("#settings-modal").modal("hide");
        });
    }
    initSettingsBtn();
}());
