const path = require('path');

module.exports = {
  //The entry point for the bundle.
  entry: './frontend/index.js',
  module: {
    loaders: [
      {test: /\.css$/, loader: 'style-loader!css-loader'},
      {test: /\.xml$/, loader: 'raw-loader'},
    ],
  },
  output: {
    //Name of the artifact produced by webpack.
    filename: 'bundle.js',
    //Locatian of the artifact - ./static/js
    path: path.join(__dirname, 'static', 'js'),
  },
};
