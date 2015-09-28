(function(window, document) {
var spa = {
    render: function() {}
}

function addEvent(el, name, f) {
    if(el.addEventListener) {
        el.addEventListener(name, f)
    } else if(el.attachEvent) {
        el.attachEvent(name, f)
    }
}

function init() {
    var candidates = []
    var fors = []
    var ifs = []
    var xs = document.body.getElementsByTagName("*")
    var len = xs.length
    var tryAdd = function(cond, dom, vs) {
        if(cond) {
            vs[vs.length] = [dom, cond]
            return true
        }
        return false
    }
    for(var i=0; i<len; ++i) {
        var x = xs[i]
        var d = x.dataset
        tryAdd(d.spaVar, x, candidates) ||
        tryAdd(d.spaIf, x, ifs) ||
        tryAdd(d.spaFor, x, fors)
    }

    var fors_len = fors.length
    var loop_vars = {}
    for(var i=0; i<fors_len; ++i) {
        var dom = fors[i][0]
        var kv = fors[i][1].split("=", 2)
        var repeat = kv[0]
        var array = kv[1]
        var template = for_template(repeat, array, dom.innerHTML)
        fors[i] = [dom, repeat, array, template]
        loop_vars[repeat] = true
    }
    var candidate_len = candidates.length
    var vars = []
    for(var i=0; i<candidate_len; ++i) {
        var x = candidates[i]
        var name = x[1].split("_")[0]
        if(!loop_vars[name]) {
            vars[vars.length] = x
        }
    }
    var vars_len = vars.length
    var ifs_len = ifs.length
    spa.render = function() {
        var app = spa.app
        for(var i=0; i<vars_len; ++i) {
            var dom = vars[i][0]
            var name = vars[i][1]
            dom.innerHTML = variable(name, app)
        }
        for(var i=0; i<ifs_len; ++i) {
            var dom = ifs[i][0]
            var name = ifs[i][1]
            dom.style.display = app[name] ? "block" : "none"
        }
        for(var i=0; i<fors_len; ++i) {
            var f = fors[i]
            var dom = f[0]
            var repeat = f[1]
            var array = f[2]
            var template = f[3]
            var xs = app[array]
            if(toString.call(xs) != "[object Array]") {
                console.log("in for loop but type=" + (toString.call(xs)) + " name=" + array)
                continue
            }
            var len = xs.length
            var html = ""
            for(var j=0; j<len; ++j) {
                html += template(j, xs[j])
            }
            dom.innerHTML = html
        }
    }
    spa.render()
}

function variable(name, dict) {
    if(typeof dict == "undefined") {
        console.log("variable dict is undefined name=" + name)
        return "(-)"
    }
    var names = name.split("_");
    var len = names.length;
    var v = dict;
    for(var i=0; i<len; ++i) {
        v = v[names[i]]
        if(typeof v == "undefined") {
            console.log("variable is undefined key=" + name + " i=" + i + " dict=" + JSON.stringify(dict))
            return "(-)"
        }
    }
    return v;
}

function for_template(repeat, array, html) {
    var xs = html.split('<span data-spa-var="' + repeat + '_')
    var vars = []
    var texts = []
    var js = []
    var len = xs.length
    for(var i=1; i<len; ++i) {
        var x = xs[i]
        var v = x.replace(/^(.+?)">.*?<\/span>/, function(_, name) {
            vars[vars.length] = name
            return ""
        })
        texts[texts.length] = v
    }
    var first = xs[0]
    return function(n, row) {
        var html = first
        var len = vars.length
        for(var i=0; i<len; ++i) {
            var v = vars[i]
            var t = texts[i]
            html += variable(v, row)
            html += t
        }
        html = html.replace('$' + repeat, 'spa.app.' + array + '[' + n + ']')
        return html
    }
}

// effect real world
addEvent(window, "load", init)
window.spa = spa;
})(window, document)

spa.app = { 
    title: "title",
    mail: "mail",
    pass: "pass",
    pages: [
        {
            title: "表現例",
            content: [
                {
                    kind: "markdown",
                    text: "# title\nmessage\n- item1\n- item2\n- item3"
                },
                {
                    kind: "text",
                    text: "# title\nmessage\n- item1\n- item2\n- item3"
                },
                {
                    kind: "link",
                    text: "http://google.co.jp"
                },
                {
                    kind: "image",
                    text: "https://www.google.co.jp/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png",
                    width: 100,
                    height: 100,
                    left: 10,
                    top: 10,
                    scale: 0.8
                },
                {
                    kind: "youtube",
                    text: "https://www.youtube.com/watch?v=ptqdqcbgxuo"
                }
            ]
        },
        {
            title: "使い方",
            content: [
                {
                    kind: "markdown",
                    text: "このツールの使い方"
                }
            ]
        }
    ]
}

function toggle(id) {
    var s = document.getElementById(id).style;
    s.display = s.display == "none" ? "block" : "none";
    return false;
}
