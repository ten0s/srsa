/*jslint
   browser: true,
   devel: true,
   nomen: true
*/
/*global _ */

"use strict";

var Storage = (function () {
    var _observers = {};

    function subscribe(key, observer) {
        if (!_observers[key]) {
            _observers[key] = [];
        }
        _observers[key].push(observer);
    }

    function notify(key) {
        _.each(_observers[key], function (observer) {
            observer(key);
        });
    }

    function setGlotIOToken(token) {
        localStorage.setItem("glot-io-token", token);
    }

    function getGlotIOToken() {
        return localStorage.getItem("glot-io-token");
    }

    function getEditorFontSize() {
        var fs = localStorage.getItem("editor-font-size");
        if (fs) {
            return parseInt(fs, 10);
        }
        return 15;
    }

    function setEditorFontSize(size) {
        localStorage.setItem("editor-font-size", size);
        notify("editor-font-size");
    }

    function onEditorFontSizeChange(observer) {
        subscribe("editor-font-size", observer);
    }

    return {
        setGlotIOToken: setGlotIOToken,
        getGlotIOToken: getGlotIOToken,

        getEditorFontSize: getEditorFontSize,
        setEditorFontSize: setEditorFontSize,
        onEditorFontSizeChange: onEditorFontSizeChange,
    };
}());
