"use strict";

// module Graphics.WebGL.Context

exports.createContext = function(canvas) {
    return function(options) {
        return function() {
            return canvas.getContext('webgl', options) ||
                   canvas.getContext("experimental-webgl", options);
        };
    };
};
