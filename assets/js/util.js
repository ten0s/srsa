function fix_relative_srcs() {
    $("[relative_src]").each(function () {
        var tag = $(this);
        var src = tag.attr("relative_src");
        tag.attr("src", relative_url(src));
    });
}

function relative_url(url) {
    var prefix = window.location.pathname.split("/")[1];
    if (prefix === "exercises") {
        return url;
    } else {
        return "/" + prefix + (url[0] === "/" ? "" : "/") + url;
    }
}
