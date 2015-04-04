'use strict'

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , browserify  = require('gulp-browserify')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , jsValidate  = require('gulp-jsvalidate')
  ;

var paths = {
    src: [
        'src/**/*.purs',
        'purescript-halogen-foundation/src/**/*.purs',
        'purescript-halogen-bootstrap/src/**/*.purs'
    ],
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
            dest: 'docs/Halogen-Component.md',
            src: 'src/Halogen/Component.purs'
        }, 
        {
            dest: 'docs/Halogen-Widgets.md',
            src: 'src/Halogen/HTML/Widget.purs'
        }, 
        {
            dest: 'docs/Halogen-HTML-Traversals.md',
            src: 'src/Halogen/HTML/Traversals.purs'
        }, 
        {
            dest: 'docs/Halogen-HTML-Renderer.md',
            src: [ 'src/Halogen/HTML/Renderer/VirtualDOM.purs', 'src/Halogen/HTML/Renderer/String.purs' ]
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
            src: ['examples/todo/Main.purs'],
            dest: 'examples/todo',
            options: {
                main: 'Example.Todo',
                modules: ['Example.Todo']
            }
        }, 
        counter: {
            src: ['examples/counter/Main.purs'],
            dest: 'examples/counter',
            options: {
                main: 'Example.Counter',
                modules: ['Example.Counter']
            }
        }, 
        ajax: {
            src: ['examples/ajax/Main.purs'],
            dest: 'examples/ajax',
            options: {
                main: 'Example.Ajax',
                modules: ['Example.Ajax']
            }
        }, 
        ace: {
            src: ['examples/ace/Main.purs'],
            dest: 'examples/ace',
            options: {
                main: 'Example.Ace',
                modules: ['Example.Ace']
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
    var docgen = purescript.pscDocs();
    docgen.on('error', function(e) {
        console.error(e.message);
        docgen.end();
    });
    return gulp.src(target.src)
        .pipe(docgen)
        .pipe(gulp.dest(target.dest));
}

gulp.task('example-todo', function() {
    return compile(purescript.psc, paths.src.concat(paths.examples.todo.src), paths.examples.todo.options)
        .pipe(browserify({}))
        .pipe(gulp.dest(paths.examples.todo.dest));
});

gulp.task('example-counter', function() {
    return compile(purescript.psc, paths.src.concat(paths.examples.counter.src), paths.examples.counter.options)
        .pipe(browserify({}))
        .pipe(gulp.dest(paths.examples.counter.dest));
});

gulp.task('example-ajax', function() {
    return compile(purescript.psc, paths.src.concat(paths.examples.ajax.src), paths.examples.ajax.options)
        .pipe(browserify({}))
        .pipe(gulp.dest(paths.examples.ajax.dest));
});

gulp.task('example-ace', function() {
    return compile(purescript.psc, paths.src.concat(paths.examples.ace.src), paths.examples.ace.options)
        .pipe(browserify({}))
        .pipe(gulp.dest(paths.examples.ace.dest));
});

gulp.task('examples', function(cb) {
    runSequence('example-todo', 'example-counter', 'example-ajax', 'example-ace', cb);
});

gulp.task('make', function() {
    return compile(purescript.pscMake, paths.src, {})
        .pipe(gulp.dest(paths.dest))
});

gulp.task('docs', function() {
    return paths.docs.forEach(function(task) {
        return docs(task);
    });
});

gulp.task('default', function(cb) {
    runSequence('make', 'docs', 'examples', cb);
});
