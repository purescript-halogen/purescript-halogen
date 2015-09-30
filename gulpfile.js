/* jshint node: true */
"use strict";

var gulp = require("gulp");
var jshint = require("gulp-jshint");
var jscs = require("gulp-jscs");
var purescript = require("gulp-purescript");
var rimraf = require("rimraf");

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
      "Data.ExistsR": "docs/Data/ExistsR.md",
      "Data.Injector": "docs/Data/Injector.md",
      "Halogen": "docs/Halogen.md",
      "Halogen.Component": "docs/Halogen/Component.md",
      "Halogen.Component.ChildPath": "docs/Halogen/Component/ChildPath.md",
      "Halogen.Driver": "docs/Halogen/Driver.md",
      "Halogen.Effects": "docs/Halogen/Effects.md",
      "Halogen.HTML": "docs/Halogen/HTML.md",
      "Halogen.HTML.Core": "docs/Halogen/HTML/Core.md",
      "Halogen.HTML.Indexed": "docs/Halogen/HTML/Indexed.md",
      "Halogen.HTML.Elements": "docs/Halogen/HTML/Elements.md",
      "Halogen.HTML.Elements.Indexed": "docs/Halogen/HTML/Elements/Indexed.md",
      "Halogen.HTML.Events": "docs/Halogen/HTML/Events.md",
      "Halogen.HTML.Events.Indexed": "docs/Halogen/HTML/Events/Indexed.md",
      "Halogen.HTML.Events.Forms": "docs/Halogen/HTML/Events/Forms.md",
      "Halogen.HTML.Events.Handler": "docs/Halogen/HTML/Events/Handler.md",
      "Halogen.HTML.Events.Types": "docs/Halogen/HTML/Events/Types.md",
      "Halogen.HTML.Properties": "docs/Halogen/HTML/Properties.md",
      "Halogen.HTML.Properties.Indexed": "docs/Halogen/HTML/Properties/Indexed.md",
      "Halogen.HTML.Properties.Indexed.Unsafe": "docs/Halogen/HTML/Properties/Indexed/Unsafe.md",
      "Halogen.HTML.Renderer.String": "docs/Halogen/HTML/Renderer/String.md",
      "Halogen.HTML.Renderer.VirtualDOM": "docs/Halogen/HTML/Renderer/VirtualDOM.md",
      "Halogen.Query": "docs/Halogen/Query.md",
      "Halogen.Query.StateF": "docs/Halogen/Query/StateF.md",
      "Halogen.Query.SubscribeF": "docs/Halogen/Query/SubscribeF.md",
      "Halogen.Util": "docs/Halogen/Util.md",
    }
  });
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task("default", ["make", "docs", "dotpsci"]);
