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

var lessUse = [];
if(isWebpackDevServer) {
  lessUse = [
    { loader: "style-loader" },
    { loader: "css-loader" },
    { loader: "less-loader" }
  ];
} else {
  plugins.push(new ExtractTextPlugin("./css/bundle.css"));
  lessUse = ExtractTextPlugin.extract({
    fallback: "style-loader",
    use: [ "css-loader", "less-loader" ]
  })
}

module.exports = {
    entry: "./client/src/index.tsx",

    output: {
        filename: "js/bundle.js",
        path: __dirname + "/client/static"
    },

    devServer: {
      contentBase: './client/static',
      port: 5022,
      stats: 'errors-only',
      proxy: {
        '/api/**': 'http://localhost:8081/'
      }
    },

    devtool: "eval-source-map",

    resolve: {
        modules: [ 'node_modules', 'bower_components' ],
        extensions: [".ts", ".tsx", ".js", ".json"]
    },

    module: {
        rules: [
            { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
            { enforce: "pre", test: /\.js$/, loader: "source-map-loader" },
            { test: /\.less$/, use: lessUse },
            { test: /\.(png|woff|woff2|eot|ttf|svg)$/, loader: 'url-loader?limit=100000' },
        ]
    },

    externals: {
        "react": "React",
        "react-dom": "ReactDOM"
    },

    plugins: [
      new webpack.LoaderOptionsPlugin({
        debug: true
      }),
      new webpack.DefinePlugin({
        'process.env.NODE_ENV': JSON.stringify('production')
      }),
      new webpack.optimize.UglifyJsPlugin()
    ].concat(plugins)
};