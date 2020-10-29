// Generated on 2015-05-04 using generator-jekyllized 0.7.3
"use strict";

var gulp = require("gulp");

var transform = require('gulp-transform');

var gulpSequence = require('gulp-sequence');

var uglify = require('gulp-uglify');

// Loads the plugins without having to list all of them, but you need
// to call them as $.pluginname
var $ = require("gulp-load-plugins")();
// "del" is used to clean out directories and such
var del = require("del");
// BrowserSync isn"t a gulp package, and needs to be loaded manually
var browserSync = require("browser-sync");

var elm = require('gulp-elm');

var fs = require('fs');
var path = require('path');
var csvParse = require('csv-parse/lib/es5');
var gitRev = require('git-rev');
var sourceStream = require('vinyl-source-stream');

// merge is used to merge the output from two different streams into the same stream
var merge = require("merge-stream");
// Need a command for reloading webpages using BrowserSync

var plumber = require("gulp-plumber");

var reload = browserSync.reload;
// And define a variable that BrowserSync uses in its function
var bs;

var wiredep = require('wiredep').stream;

const {
  exec
} = require('child_process');


// Deletes the directory that is used to serve the site during development
gulp.task("clean:dev", function(cb) {
  return del(["serve"],
    cb);
});


// Deletes the directory that the optimized site is output to
gulp.task("clean:prod", function(cb) {
  return del(["dist"], cb);
});


// Compiles the SASS files and moves them into the "assets/stylesheets" directory
gulp.task("styles", [], function() {
  // Looks at the style.scss file for what to include and creates a style.css file
  return gulp.src("src/assets/scss/style.scss")
    .pipe(plumber())
    .pipe($.sass())
    .on('error', function(err) {
      browserSync.notify("SASS error");

      console.error(err.message);

      // Save the error to index.html, with a simple HTML wrapper
      // so browserSync can inject itself in.
      fs.writeFileSync('serve/index.html',
        "<!DOCTYPE HTML><html><body><pre>" + err.message +
        "</pre></body></html>");

      // No need to continue processing.
      this.emit('end');
    })
    // AutoPrefix your CSS so it works between browsers
    .pipe($.autoprefixer("last 1 version", {
      cascade: true
    }))
    // Directory your CSS file goes to
    .pipe(gulp.dest("serve/assets/stylesheets/"))
    // Outputs the size of the CSS file
    .pipe($.size({
      title: "styles"
    }))
    // Injects the CSS changes to your browser since Jekyll doesn"t rebuild the CSS
    .pipe(reload({
      stream: true
    }));
});

// Compile the raw Z-Score files to something more useful.
gulp.task("zscore", [], function() {
  var parseOptions = {
    cast: true,
    columns: true,
    delimiter: "\t",
    trim: true
  };

  return gulp.src("src/assets/z-score/*.txt")
    .pipe(transform('utf8', function(content) {
      return new Promise((resolve, reject) => {
        csvParse(content, parseOptions, function(err, result) {
          if (err) {
            reject(err);
          } else {
            resolve(JSON.stringify(result));
          }
        });
      });
    })).pipe($.rename({
      extname: '.json'
    }))
    // Copy the JSON to a place the Elm client can get it via HTTP
    .pipe(gulp.dest('serve/assets/z-score/'))
    // And copy it to a place where the backend can also get it
    .pipe(gulp.dest('../server/hedley/modules/custom/hedley_zscore/json/'))
    // And turn the JSON into an Elm fixture for testing
    .pipe(transform('utf8', function(content, file) {
      var moduleName = capitalize(path.basename(file.path, '.json'));
      return `module ZScore.Fixture.${moduleName} exposing (..)\n\n\njson : String\njson =\n    """${content}"""`;
    })).pipe($.rename(function(path) {
      path.basename = capitalize(path.basename);
      path.extname = '.elm';
    })).pipe(gulp.dest('src/generated/ZScore/Fixture/'));
});

gulp.task('version', [], function() {
  var stream = sourceStream('Version.elm');

  gitRev.short(function(rev) {
    stream.write('module Version exposing (version)\n');
    stream.write('import App.Model exposing (Version)\n');
    stream.write('version : Version\n');
    stream.write('version = {build = "' + rev + '"}\n');
    stream.end();
  });

  return stream.pipe(gulp.dest('src/generated'));
});

function capitalize(input) {
  return input.charAt(0).toUpperCase() + input.slice(1);
}

// Optimizes the images that exists
gulp.task("images", function() {
  return gulp.src("src/assets/images/**")
    .pipe($.changed("dist/assets/images"))
    .pipe($.imagemin({
      // Lossless conversion to progressive JPGs
      progressive: true,
      // Interlace GIFs for progressive rendering
      interlaced: true
    }))
    .pipe(gulp.dest("dist/assets/images"))
    .pipe($.size({
      title: "images"
    }));
});

// Copy fonts.
gulp.task("copy:fonts", function() {
  return gulp.src("src/assets/fonts/**")
    .pipe(gulp.dest("serve/assets/fonts"))
    .pipe($.size({
      title: "fonts"
    }));
});

// Copy index.html and CNAME files to the "serve" directory
gulp.task("copy:dev", ["copy:bower", "copy:images", "copy:favicon",
  "copy:fonts"
], function() {
  return gulp.src(["src/index.html", "src/CNAME", "src/js/**/*"])
    .pipe(gulp.dest("serve"))
    .pipe($.size({
      title: "index.html & CNAME"
    }))
});

// Copy bower.
gulp.task("copy:bower", function() {
  // There are unused Dexie files that causes trouble for uglify later
  return gulp.src([
      "bower_components/**/*",
      "!bower_components/**/*.es.js",
      "!bower_components/semantic/tasks/config/admin/templates/*.js"
    ]).pipe(gulp.dest("serve/bower_components"))
    .pipe($.size({
      title: "Bower"
    }))
});

// Copy images.
gulp.task("copy:images", function() {
  return gulp.src(["src/assets/images/**/*"])
    .pipe(gulp.dest("serve/assets/images"))
    .pipe($.size({
      title: "Assets images"
    }))
});

// Copy favicon stuff
gulp.task("copy:favicon", function() {
  return gulp.src(["src/assets/favicon/**/*"])
    .pipe(gulp.dest("serve/"))
    .pipe($.size({
      title: "Assets favicon"
    }));
});

gulp.task("cname", function() {
  return gulp.src(["serve/CNAME"])
    .pipe(gulp.dest("dist"))
    .pipe($.size({
      title: "CNAME"
    }))
});

gulp.task('bower', function() {
  gulp.src("src/index.html")
    .pipe(wiredep())
    .pipe(gulp.dest("serve"));
});


// Optimizes all the CSS, HTML and concats the JS etc
gulp.task("minify", ["styles", "zscore", "copy:images", "copy:favicon"],
  function() {
    return gulp.src("serve/**/*.*")
      // Concatenate JavaScript files and preserve important comments.
      // DropZone had a problem if we mangle
      // ... see <https://github.com/rowanwins/vue-dropzone/issues/119>
      .pipe($.if(["*.js", "!service-worker.js", "!workbox-*.js", "!app.js", "!nodes.js", "!photos.js", "!statistics.js"], uglify({
        mangle: false
      }))).on('error', function(err) {
        console.error(err);
      })

    // Minify CSS
    .pipe($.if("*.css", $.minifyCss()))

    // We don't do cache-busting here because the PWA stuff makes that obsolete

    // Minify HTML
    .pipe($.if("*.html", $.htmlmin({
      removeComments: true,
      removeCommentsFromCDATA: true,
      removeCDATASectionsFromCDATA: true,
      collapseWhitespace: true,
      collapseBooleanAttributes: true,
      removeAttributeQuotes: true,
      removeRedundantAttributes: true
    })))

    // Send the output to the correct folder
    .pipe(gulp.dest("dist"))
      .pipe($.size({
        title: "optimizations"
      }));
  });


// Task to upload your site to your GH Pages repo
gulp.task("deploy", [], function() {
  // Deploys your optimized site, you can change the settings in the html task if you want to
  return gulp.src("dist/**/*")
    .pipe($.ghPages({
      branch: "gh-pages"
    }));
});

gulp.task('elm-init', elm.init);
gulp.task('elm', ['elm-init', 'version'], function() {
  return gulp.src('src/elm/Main.elm')
    .pipe(plumber())
    .pipe(elm({
      'debug': false,
      'warn': false
    }))
    .on('error', function(err) {
      console.error(err.message);

      browserSync.notify("Elm compile error", 5000);

      // Save the error to index.html, with a simple HTML wrapper
      // so browserSync can inject itself in.
      fs.writeFileSync('serve/index.html',
        "<!DOCTYPE HTML><html><body><pre>" + err.message +
        "</pre></body></html>");
    })
    .pipe(gulp.dest('serve'));
});

// BrowserSync will serve our site on a local server for us and other devices to use
// It will also autoreload across all devices as well as keep the viewport synchronized
// between them.
gulp.task("serve:dev", ["build"], function() {
  bs = browserSync({
    notify: true,
    // tunnel: "",
    server: {
      baseDir: "serve"
    }
  });
});

// Serves on HTTPS for the Android emulator.
gulp.task("serve:emulator", ["build", "ssl-cert"], function() {
  bs = browserSync({
    notify: true,
    // tunnel: "",
    server: {
      baseDir: "serve"
    },
    https: {
      cert: "ssl/ssl.pem",
      key: "ssl/ssl.key"
    }
  });
});

// Makes an SSL certificate for ihangane.dev for the Android emulator. Just
// run this once, and then do what is necessary to trust it.
gulp.task("ssl-cert", function(cb) {
  fs.access("ssl/ssl.crt", fs.constants.F_OK, (err) => {
    if (err) {
      // Doesn't exist, so create cert.
      const cmdGen =
        "openssl req -batch -config ssl/ihangane.dev.conf -new -sha256 -newkey rsa:2048 -nodes -x509 -days 1024 -keyout ssl/ssl.key -out ssl/ssl.pem";
      exec(cmdGen, {}, function(err, stdout, stderr) {
        console.log(stdout);
        console.log(stderr);
        const cmdConvert =
          "openssl x509 -in ssl/ssl.pem -inform pem -out ssl/ssl.crt -outform der";
        exec(cmdConvert, {}, cb);
      });
    } else {
      // exists, so do nothing.
      cb();
    }
  });
});

// These tasks will look for files that change while serving and will auto-regenerate or
// reload the website accordingly. Update or add other files you need to be watched.
gulp.task("watch", function() {
  // We need to copy dev, so index.html may be replaced by error messages.
  gulp.watch(["src/index.html", "src/js/**/*.js"], ["copy:dev",
    "pwa:dev",
    reload
  ]);
  gulp.watch(["src/elm/**/*.elm"], ["elm", "copy:dev",
    "pwa:dev", reload
  ]);
  gulp.watch(["src/assets/scss/**/*.scss"], ["styles", "copy:dev",
    "pwa:dev", reload
  ]);
  gulp.watch(["src/assets/zscore/**/*.txt"], ["zscore", "copy:dev",
    "pwa:dev", reload
  ]);
});

// Serve the site after optimizations to see that everything looks fine
gulp.task("serve:prod", function() {
  bs = browserSync({
    notify: false,
    // tunnel: true,
    server: {
      baseDir: "dist"
    }
  });
});

var precacheFileGlob =
  '*.{js,html,css,png,jpg,gif,svg,eot,ttf,woff,woff2,json}';

// We cache bower_components individually, since they often include things
// we don't need.
var precacheLocalDev = [
  precacheFileGlob,
  'assets/**/' + precacheFileGlob,
  'bower_components/copy-button/bundled.min.js',
  'bower_components/dropzone/dist/min/dropzone.min.css',
  'bower_components/dropzone/dist/min/dropzone.min.js',
  'bower_components/dexie/dist/dexie.min.js',
  'bower_components/semantic/dist/themes/**/' + precacheFileGlob,
  'bower_components/semantic/dist/semantic.min.css'
];

// There may be a better way to do this, but for the moment we have some
// slight duplication here.
var precacheProd = [
  precacheFileGlob,
  'assets/**/' + precacheFileGlob,
  'bower_components/copy-button/bundled.min.*.js',
  'bower_components/dropzone/dist/min/dropzone.min.*.css',
  'bower_components/dropzone/dist/min/dropzone.min.*.js',
  'bower_components/dexie/dist/dexie.min.*.js',
  'bower_components/semantic/dist/themes/**/' + precacheFileGlob,
  'bower_components/semantic/dist/semantic.min.*.css'
];

// For offline use while developing
gulp.task('pwa:dev', ["styles", "zscore", "copy:dev", "elm"], function() {
  var workboxBuild = require('workbox-build');

  return workboxBuild.generateSW({
    swDest: 'serve/service-worker.js',
    cacheId: 'ihangane',
    globDirectory: 'serve',
    globPatterns: precacheLocalDev,
    maximumFileSizeToCacheInBytes: 20 * 1024 * 1024,
    clientsClaim: true,
    skipWaiting: false,
    importScripts: [
      'bower_components/dexie/dist/dexie.min.js',
      'uuid.js',
      'sw.js',
      'config.js',
      'lifecycle.js',
      'nodes.js',
      'statistics.js',
      'photos.js',
    ]
  });
});

// Offline use in production.
gulp.task('pwa:prod', function() {
  var workboxBuild = require('workbox-build');

  return workboxBuild.generateSW({
    swDest: 'dist/service-worker.js',
    cacheId: 'ihangane',
    globDirectory: 'dist',
    globPatterns: precacheProd,
    maximumFileSizeToCacheInBytes: 20 * 1024 * 1024,
    clientsClaim: true,
    skipWaiting: false,
    importScripts: [
      'bower_components/dexie/dist/dexie.min.js',
      'uuid.js',
      'sw.js',
      'config.js',
      'lifecycle.js',
      'nodes.js',
      'statistics.js',
      'photos.js',
    ]
  });
});

// Serve for the Android emulator, then watch.
gulp.task("emulator", ["serve:emulator", "watch"]);

// Default task, run when just writing "gulp" in the terminal
gulp.task("default", ["serve:dev", "watch"]);

// Builds the site but doesnt serve it to you
// @todo: Add "bower" here
gulp.task("build", gulpSequence("clean:dev", ["styles",
  "zscore", "copy:dev",
  "elm", "pwa:dev"
]));

// Builds your site with the "build" command and then runs all the optimizations on
// it and outputs it to "./dist"
gulp.task("publish", gulpSequence(["build", "clean:prod"], ["minify", "cname",
  "images"
], "pwa:dev"));
