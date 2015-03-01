'use strict'

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , browserify  = require('gulp-browserify')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , jsValidate  = require('gulp-jsvalidate')
  ;

var paths = {
    src: 'src/**/*.purs',
    bowerSrc: [
      'bower_components/purescript-*/src/**/*.purs'
    ],
    dest: '',
    docs: {
        'all': {
            dest: 'MODULES.md',
            src: [
              'src/Halogen.purs',
              'src/Halogen/HTML.purs',
              'src/Halogen/Signal.purs'
            ]
        }
    },
    testSrc: 'test/**/*.purs'
};

var testOpts = {
    main: 'Test.Main',
    modules: ['Test.Main']
};

function compile (compiler, src, opts) {
    var psc = compiler(opts);
    psc.on('error', function(e) {
        console.error(e.message);
        psc.end();
    });
    return gulp.src(src.concat(paths.bowerSrc))
        .pipe(psc)
        .pipe(jsValidate());
};

function docs (target) {
    return function() {
        var docgen = purescript.pscDocs();
        docgen.on('error', function(e) {
            console.error(e.message);
            docgen.end();
        });
        return gulp.src(paths.docs[target].src)
            .pipe(docgen)
            .pipe(gulp.dest(paths.docs[target].dest));
    }
}

gulp.task('example', function() {
    return compile(purescript.psc, [paths.src].concat(paths.bowerSrc).concat(paths.testSrc), testOpts)
        .pipe(browserify({}))
        .pipe(gulp.dest('js'))
});

gulp.task('make', function() {
    return compile(purescript.pscMake, [paths.src].concat(paths.bowerSrc), {})
        .pipe(gulp.dest(paths.dest))
});

gulp.task('docs', docs('all'));

gulp.task('default', function(cb) {
  runSequence('make', 'docs', 'example', cb);
});
