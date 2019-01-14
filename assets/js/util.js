function fix_img_relative_srcs() {
    $("img[relative_src]").each(function () {
        var img = $(this);
        var src = img.attr("relative_src");
        img.attr("src", relative_url(src));
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
