'use strict';

module.exports = {
  entry: './entry',

  mode: 'development',

  devServer: {
    contentBase: '.',
    port: 1337,
    stats: 'errors-only'
  },

  output: {
    path: __dirname,
    pathinfo: true,
    filename: 'bundle.js'
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
                'src/**/*.purs'
              ],
              bundle: false,
              psc: 'psa',
              watch: true,
              pscIde: false
            }
          }
        ]
      },
      {
        test: /\.css$/,
        use: [
          {
            loader: 'style-loader',
          },
          {
            loader: 'css-loader',
          }
        ]
      },
      {
        test: /\.svg$/,
        use: [
          {
            loader: 'file-loader',
          },
        ]
      },
    ]
  },


  resolve: {
    modules: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['.purs', '.js']
  }
}