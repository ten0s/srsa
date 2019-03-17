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

    function setCORSAnywhereUrl(url) {
        if (url[url.length - 1] !== "/") {
            url += "/";
        }
        localStorage.setItem("cors-anywhere-url", url);
    }

    function getCORSAnywhereUrl() {
        var url = localStorage.getItem("cors-anywhere-url");
        if (!url) {
            url = "https://cors-anywhere.herokuapp.com/";
        }
        return url;
    }

    function getEditorFontSize() {
        var fs = localStorage.getItem("editor-font-size");
        if (fs) {
            return parseInt(fs, 10);
        }
        return 15;
    }

    function setEditorFontSize(size) {
        var oldSize = getEditorFontSize();
        localStorage.setItem("editor-font-size", size);
        if (oldSize !== size) {
            notify("editor-font-size");
        }
    }

    function onEditorFontSizeChange(observer) {
        subscribe("editor-font-size", observer);
    }

    function getEditorTheme() {
        var theme = localStorage.getItem("editor-theme");
        if (!theme) {
            return "ace/theme/monokai";
        }
        return theme;
    }

    function setEditorTheme(theme) {
        var oldTheme = getEditorTheme();
        localStorage.setItem("editor-theme", theme);
        if (oldTheme !== theme) {
            notify("editor-theme");
        }
    }

    function onEditorThemeChange(observer) {
        subscribe("editor-theme", observer);
    }

    return {
        setGlotIOToken: setGlotIOToken,
        getGlotIOToken: getGlotIOToken,
        setCORSAnywhereUrl: setCORSAnywhereUrl,
        getCORSAnywhereUrl: getCORSAnywhereUrl,

        getEditorFontSize: getEditorFontSize,
        setEditorFontSize: setEditorFontSize,
        onEditorFontSizeChange: onEditorFontSizeChange,

        getEditorTheme: getEditorTheme,
        setEditorTheme: setEditorTheme,
        onEditorThemeChange: onEditorThemeChange,
    };
}());
