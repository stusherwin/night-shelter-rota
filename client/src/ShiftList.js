'use strict';

exports.scrollTop = function() {
  return document.body.scrollTop;
}

exports.scrollTo = function(scrollTop) {
  return function() {
    document.body.scrollTop = scrollTop;
  }
}

exports.elementOffsetTopById = function(id) {
  return function() {
    var el = document.getElementById(id);
    var offsetTop = el ? el.offsetTop : 0;
    return offsetTop;
  }
}

exports.firstElementIdByClassName = function(className) {
  return function() {
    var els = document.getElementsByClassName(className);
    var id = els.length ? els[0].id : "";
    return id;
  };
}

exports.lastElementIdByClassName = function(className) {
  return function() {
    var els = document.getElementsByClassName(className);
    var id = els.length ? els[els.length - 1].id : "";
    return id;
  }
}