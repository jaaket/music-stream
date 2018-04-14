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

  // module: {
  //   rules: [
  //     {
  //       test: /\.purs$/,
  //       loader: 'purs-loader',
  //       exclude: /node_modules/,
  //       query: {
  //         src: [
  //           'bower_components/purescript-*/src/**/*.purs',
  //           'src/**/*.purs'
  //         ]
  //       }
  //     }
  //   ]
  // },

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
              psc: 'psc',
              watch: true,
              pscIde: false
            }
          }
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