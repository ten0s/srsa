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

function fold_ranges(text) {
    var lines = text.split("\n");
    var ranges = [];
    var begin = -1;
    _.each(lines, function (line, number) {
        if (/\+BEGIN_FOLD/.exec(line)) {
            //console.log("beg: " + number);
            begin = number;
        }
        if (/\+END_FOLD/.exec(line)) {
            //console.log("end: " + number);
            ranges.push(new Array(begin, number));
        }
    });
    //console.log(ranges);
    return ranges;
}
