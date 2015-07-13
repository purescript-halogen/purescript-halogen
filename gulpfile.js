/* jshint node: true */
"use strict";

var gulp = require("gulp");
var jshint = require("gulp-jshint");
var jscs = require("gulp-jscs");
var purescript = require("gulp-purescript");
var rimraf = require("rimraf");
var webpack = require("webpack-stream");

var sources = [
  "src/**/*.purs",
  "test/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "test/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("lint", function() {
  return gulp.src("src/**/*.js")
    .pipe(jshint())
    .pipe(jshint.reporter())
    .pipe(jscs());
});

gulp.task("make", ["lint"], function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("docs", ["clean-docs"], function() {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      "Halogen": "docs/Halogen.md",
      "Halogen.Component": "docs/Halogen/Component.md",
      "Halogen.Effects": "docs/Halogen/Effects.md",
      "Halogen.HTML": "docs/Halogen/HTML.md",
      "Halogen.HTML.CSS": "docs/Halogen/HTML/CSS.md",
      "Halogen.HTML.Events": "docs/Halogen/HTML/Events.md",
      "Halogen.HTML.Events.Forms": "docs/Halogen/HTML/Events/Forms.md",
      "Halogen.HTML.Events.Handler": "docs/Halogen/HTML/Events/Handler.md",
      "Halogen.HTML.Events.Types": "docs/Halogen/HTML/Events/Types.md",
      "Halogen.HTML.Properties": "docs/Halogen/HTML/Properties.md",
      "Halogen.HTML.Renderer.String": "docs/Halogen/HTML/Renderer/String.md",
      "Halogen.HTML.Renderer.VirtualDOM": "docs/Halogen/HTML/Renderer/VirtualDOM.md",
    }
  });
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task("prebundle", ["make"], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "tmp/test.js",
    module: "Test.Main",
    main: "Test.Main"
  });
});

gulp.task("bundle", ["prebundle"], function () {
  return gulp.src("tmp/test.js")
    .pipe(webpack({
      resolve: { modulesDirectories: ["node_modules"] },
      output: { filename: "test.js" }
    }))
    .pipe(gulp.dest("tmp"));
});

gulp.task("default", ["bundle", "docs", "dotpsci"]);
