'use strict';

const path = require('path');

const webpack = require('webpack');

const isWebpackDevServer = process.argv.filter(a => path.basename(a).indexOf('webpack-dev-server') >= 0).length;
const isWatch = process.argv.filter(a => a === '--watch').length

var ExtractTextPlugin = require("extract-text-webpack-plugin");

var plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

var jsOutputFilename = isWebpackDevServer ? './js/bundle.js' : './client/static/js/bundle.js'

var lessUse = [];
if(isWebpackDevServer) {
  lessUse = [
    { loader: "style-loader" },
    { loader: "css-loader" },
    { loader: "less-loader" }
  ];
} else {
  plugins.push(new ExtractTextPlugin("./client/static/css/bundle.css"));
  lessUse = ExtractTextPlugin.extract({
    fallback: "style-loader",
    use: [ "css-loader", "less-loader" ]
  })
}

module.exports = {
  devtool: 'eval-source-map',

  devServer: {
    contentBase: './client/static',
    port: 5022,
    stats: 'errors-only',
    proxy: {
      '/api/**': 'http://localhost:8081/'
    }
  },

  entry: './client/src/Main.purs',

  output: {
    path: __dirname,
    pathinfo: true,
    filename: jsOutputFilename
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'bower_components/purescript-*/src/**/*.purs',
                'client/src/**/*.purs'
              ],
              bundle: false,
              psc: 'psa',
              pscArgs: { censorCodes: "WildcardInferredType" },
              watch: isWebpackDevServer || isWatch,
              pscIde: false
            }
          }
        ]
      }, {
        test: /\.less$/,
        use: lessUse
      }, {
        test: /\.(png|woff|woff2|eot|ttf|svg)$/,
        loader: 'url-loader?limit=100000'
      }
    ]
  },

  resolve: {
    modules: [ 'node_modules', 'bower_components' ],
    extensions: [ '.purs', '.js']
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    })
  ].concat(plugins)
};
