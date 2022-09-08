'use strict';

// module Lib.Window.Main

exports.requestAnimationFrameImpl = function (fn) {
  return function (w) {
    return function () {
      return w.requestAnimationFrame((t) => fn(t)());
    };
  };
};

exports.cancelAnimationFrameImpl = function (id) {
  return function (w) {
    return function () {
      return w.cancelAnimationFrame(id);
    };
  };
};
