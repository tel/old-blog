var Le = (function () {
    "use strict"

    
    var 
    
    
    var X = {};

    X.f = 5;

    // Debug mode
    X.z = function () {
        require.cache = {};
        return require("./miniLens.js")
    }

    return X;

})();
