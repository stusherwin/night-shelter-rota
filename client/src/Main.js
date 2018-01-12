'use strict';

require('../static/less/Style.less')

exports.isServerSide = typeof document === 'undefined';

exports.getElementById = function(id) {
  return function(){
    return document.getElementById(id);
  };
};

exports.hot = function() {
  if (module.hot) {
    module.hot.accept();
  }
}

exports.documentOnClick = function(handler) {
  return function() {
    document.addEventListener('click', function() { handler(); });
  }
}