/* jshint node: true */
"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var webpack = require("webpack-stream");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("make", function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("prebundle", ["make"], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "dist/example.js",
    module: "Example.Counter",
    main: "Example.Counter"
  });
});

gulp.task("bundle", ["prebundle"], function () {
  return gulp.src("dist/example.js")
    .pipe(webpack({
      resolve: { modulesDirectories: ["node_modules"] },
      output: { filename: "example.js" }
    }))
    .pipe(gulp.dest("dist"));
});

gulp.task("default", ["bundle"]);

