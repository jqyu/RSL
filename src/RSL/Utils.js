"use strict";

// module RSL.Utils

exports.mkExistsK = function (fa) {
  return fa
};

exports.runExistsK = function (f) {
  return function (fa) {
    return f(fa);
  }
}
