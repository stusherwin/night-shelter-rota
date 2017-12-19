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