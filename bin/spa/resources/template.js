function(window_) {

var spa = {
    _var: {},
    _dom: {},
    _render: {},
    init: function() {
        var elements = document.getElementsByTagName("span");
        var len = elements.length;
        var dom = {};
        for(var i=0; i<len; ++i) {
            var e = elements[i];
            var name = e.dataset.spa;
            if(name) {
                if(dom[name]) {
                    var doms = dom[name];
                    doms[doms.length] = e;
                } else {
                    dom[name] = [e];
                }
            }
        }

        // update this
        this._var = {};
        this._dom = dom;
        this._render = {};
    },
    update: function(vars) {
        var dom = this._dom;
        var render = this._render;
        for(var name in vars) {
            this._var[name] = vars[name];

            var func = render[name];
            var doms = dom[name];
            if(func && doms) {
                var len = doms.length;
                var html = func();
                for(var i=0; i<len; ++i) {
                    doms[i].innerHTML = html;
                }
            }
        }
    }
};

// effects global
window_.addEventListener("load", function(){ spa.init(); });
window_.spa = spa;
}(window);
