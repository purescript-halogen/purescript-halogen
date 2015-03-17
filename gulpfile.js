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
    docs: [ 
        {
            dest: 'docs/Halogen.md',
            src: 'src/Halogen.purs'
        }, 
        {
            dest: 'docs/Halogen-Signal.md',
            src: 'src/Halogen/Signal.purs'
        }, 
        {
            dest: 'docs/Halogen-HTML.md',
            src: [ 'src/Halogen/HTML.purs', 'src/Halogen/HTML/Attributes.purs' ]
        }, 
        {
            dest: 'docs/Halogen-Forms.md',
            src: 'src/Halogen/HTML/Events/Forms.purs'
        }, 
        {
            dest: 'docs/Halogen-Target.md',
            src: 'src/Halogen/HTML/Target.purs'
        }, 
        {
            dest: 'docs/Halogen-Events.md',
            src: [ 'src/Halogen/HTML/Events/Types.purs', 'src/Halogen/HTML/Events.purs', 'src/Halogen/HTML/Events/Handler.purs' ]
        }, 
        {
            dest: 'docs/Halogen-Mixin-UndoRedo.md',
            src: 'src/Halogen/Mixin/UndoRedo.purs'
        }, 
        {
            dest: 'docs/Halogen-Mixin-Router.md',
            src: 'src/Halogen/Mixin/Router.purs'
        }, 
        {
            dest: 'docs/Halogen-Mixin-Aff.md',
            src: 'src/Halogen/Mixin/Aff.purs'
        }, 
        {
            dest: 'docs/Halogen-Bootstrap.md',
            src: 'purescript-halogen-bootstrap/src/Halogen/Themes/Bootstrap3.purs'
        },  
        {
            dest: 'docs/Halogen-Bootstrap-Controls.md',
            src: 'purescript-halogen-bootstrap/src/Halogen/Themes/Bootstrap3/*.purs'
        }, 
        {
            dest: 'docs/Halogen-Foundation.md',
            src: 'purescript-halogen-foundation/src/**/*.purs'
        }
    ],
    examples: {
        todo: {
            src: ['examples/todo/Main.purs', 'purescript-halogen-bootstrap/src/**/*.purs'],
            dest: 'examples/todo',
            options: {
                main: 'Example.Todo',
                modules: ['Example.Todo']
            }
        }, 
        counter: {
            src: ['examples/counter/Main.purs', 'purescript-halogen-bootstrap/src/**/*.purs'],
            dest: 'examples/counter',
            options: {
                main: 'Example.Counter',
                modules: ['Example.Counter']
            }
        }, 
        ajax: {
            src: ['examples/ajax/Main.purs', 'purescript-halogen-bootstrap/src/**/*.purs'],
            dest: 'examples/ajax',
            options: {
                main: 'Example.Ajax',
                modules: ['Example.Ajax']
            }
        }
    }
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
        return gulp.src(target.src)
            .pipe(docgen)
            .pipe(gulp.dest(target.dest));
    }
}

gulp.task('example-todo', function() {
    return compile(purescript.psc, [paths.src].concat(paths.examples.todo.src), paths.examples.todo.options)
        .pipe(browserify({}))
        .pipe(gulp.dest(paths.examples.todo.dest));
});

gulp.task('example-counter', function() {
    return compile(purescript.psc, [paths.src].concat(paths.examples.counter.src), paths.examples.counter.options)
        .pipe(browserify({}))
        .pipe(gulp.dest(paths.examples.counter.dest));
});

gulp.task('example-ajax', function() {
    return compile(purescript.psc, [paths.src].concat(paths.examples.ajax.src), paths.examples.ajax.options)
        .pipe(browserify({}))
        .pipe(gulp.dest(paths.examples.ajax.dest));
});

gulp.task('examples', function(cb) {
    runSequence('example-todo', 'example-counter', 'example-ajax', cb);
});

gulp.task('make', function() {
    return compile(purescript.pscMake, [paths.src], {})
        .pipe(gulp.dest(paths.dest))
});

function runInParallel(tasks, cb) {
    var count = 0;
    tasks.forEach(function(task) {
        task(function() {
            
            if (count++ == tasks.length) {
                cb();
            }
        });
    });
}

gulp.task('docs', function(cb) {
    return runInParallel(paths.docs.map(function(task) {
        return docs(task);
    }), cb);
});

gulp.task('default', function(cb) {
    runSequence( 'examples', cb);
});
