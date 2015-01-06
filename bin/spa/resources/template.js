var var_template = document.createDocumentFragment("span");
var template = {};
var env = {};
var dom = {};

function createHtml(tag, attrs, xs) {
    var node = document.createDocumentFragment(tag);
    var len_attr = attrs.length;
    for(var i=0; i<len_attr; ++i)
    {
        var kv = attrs[i];
        t.setAttribute(kv[0], kv[1]);
    }
    var len_x = xs.length;
    for(var i=0; i<len_x; ++i)
    {
        var x = xs[i];
        node.appendChild(createHtml(x[0], x[1], x[2]));
    }
    return function(env) {
        return node;
    }
}

function createVar(name) {
    return function(env, me) {
        me.textContent = env[name];
    }
}

function createIf(name) {
    return function(env, me) {
        me.style.display = env[name] ? "block" : "none";
    }
}

function createFor(name) {
    return function(env, me) {
        var xs = env[name];
        var len = xs.length;
        var nodes = document.createDocumentFragment("div");
        for(var i=0; i<len; ++i) {
            nodes.appendChild(node.cloneNode(true));
        }
    }
}
