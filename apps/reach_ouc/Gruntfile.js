// Generated on 2014-02-11 using generator-webapp 0.4.7
'use strict';

// # Globbing
// for performance reasons we're only matching one level down:
// 'test/spec/{,*/}*.js'
// use this if you want to recursively match all subfolders:
// 'test/spec/**/*.js'

module.exports = function (grunt) {

  // Load grunt tasks automatically
  require('load-grunt-tasks')(grunt);

  // Time how long tasks take. Can help when optimizing build times
  require('time-grunt')(grunt);

  var srcdir = grunt.option('srcdir') || '.';
  var builddir = grunt.option('builddir') || '.';

  // Define the configuration for all the tasks
  grunt.initConfig({

    // Project settings
    settings: {
      // Configurable paths
      app: srcdir + '/site',
      dev: srcdir + '/priv/www',
      dist: builddir + '/site_dist'
    },

    // Watches files for changes and runs tasks based on the changed files
    watch: {
      scripts: {
        files: [
          '<%= settings.app %>/scripts/**/*.js',
          '<%= settings.app %>/gadgets/*/scripts/*.js',
          '<%= settings.app %>/gadgets/*/meta/*.json',
          '<%= settings.app %>/gadgets/*/templates/*.html',
          '<%= settings.app %>/gadgets/test-page.js',
          '<%= settings.app %>/gadgets/{,*/}index.html'
        ],
        tasks: ['newer:copy:dev']
      },
      gruntfile: {
        files: ['Gruntfile.js']
      },
      styles: {
        files: ['<%= settings.app %>/styles/*.css'],
        tasks: ['newer:copy:dev']
      },
      less: {
        files: ['<%= settings.app %>/gadgets/{,*/}styles/{,*/}*.less'],
        tasks: ['less:dev']
      }
    },

    // Empties folders to start fresh
    clean: {
      dist: {
        files: [{
          dot: true,
          src: [
            '.tmp',
            '<%= settings.dist %>/*',
            '!<%= settings.dist %>/.git*'
          ]
        }]
      },
      dev: {
        files: [{
          dot: true,
          src: [
            '<%= settings.dev %>/*',
            '!<%= settings.dev %>/.git*'
          ]
        }]
      }
    },

    less: {
      options: {
        compile: true
      },
      dev: {
        files: [{
          expand: true,
          cwd: '<%= settings.app %>/gadgets/',
          src: '*/styles{,*/}*.less',
          dest: '<%= settings.dev %>/gadgets/',
          ext: '.css'
        }]
      },
      dist: {
        options: {
          cleancss: true
        },
        files: [{
          expand: true,
          cwd: '<%= settings.app %>/gadgets/',
          src: '*/styles{,*/}*.less',
          dest: '<%= settings.dist %>/gadgets/',
          ext: '.css'
        }]
      }
    },

    requirejs: {
      options: {
        dir: '.tmp',
        appDir: '<%= settings.app %>',
        baseUrl: 'scripts/app',
        paths: {
          'amplify': '../lib/amplify/amplify',
          'bootstrap': '../lib/bootstrap/bootstrap',
          'bootstrap-tabgrp': '../lib/bootstrap/plugins/bootstrap-tabgrp',
          'bootbox': '../lib/bootstrap/plugins/bootbox',
          'buzz': '../lib/buzz/buzz',
          'dateFormat': '../lib/core/date.format',
          'sprintf': '../lib/core/sprintf',
          'jquery': '../lib/jquery/jquery',
          'jquery-migrate': '../lib/jquery/jquery-migrate',
          'jquery-ui': '../lib/jquery/jquery-ui',
          'bbq': '../lib/jquery/plugins/jquery.ba-bbq',
          'resize': '../lib/jquery/plugins/jquery.ba-resize',
          'cookie': '../lib/jquery/plugins/jquery.cookie',
          'gridster': '../lib/jquery/plugins/jquery.gridster',
          'simplePagination': '../lib/jquery/plugins/jquery.simplePagination',
          'knockout': '../lib/knockout/knockout',
          'ko-mapping': '../lib/knockout/plugins/mapping',
          'ko-classBindingProvider': '../lib/knockout/plugins/classBindingProvider',
          'less': '../lib/less/less.min',
          'domReady': '../lib/require/plugins/domReady',
          'text': '../lib/require/plugins/text',
          'json': '../lib/require/plugins/json',
          'strophe': '../lib/strophe/strophe',
          'underscore': '../lib/underscore/underscore',
          'defaults': '../defaults',
          'templates': '../../templates',
          'gadgets': '../../gadgets'
        },
        shim: {
          'jquery-migrate': ['jquery'],

          'jquery-ui': ['jquery', 'jquery-migrate'],

          'bootstrap': ['jquery'],

          'bootbox': ['jquery', 'bootstrap'],

          'bootstrap-tabgrp': ['jquery', 'bootstrap'],

          'bbq': ['jquery', 'jquery-migrate'],

          'resize': ['jquery'],

          'amplify': ['jquery'],

          'cookie': ['jquery'],

          'gridster': ['jquery'],

          'simplePagination': ['jquery'],

          'ko-mapping': ['knockout'],

          'ko-classBindingProvider': ['knockout'],

          'defaults': ['jquery', 'jquery-migrate', 'jquery-ui', 'underscore']
        },
        modules: [{
          name: '../init',
          include: [
            'domReady',
            'text',
            'json',
            'dateFormat',
            'jquery',
            'jquery-ui',
            'underscore',
            'amplify',
            'bootstrap',
            'bootstrap-tabgrp',
            'bootbox',
            'bbq',
            'resize',
            'cookie',
            'gridster',
            'simplePagination',
            'buzz',
            'defaults'
          ]
        }, {
          name: '../login-page',
          exclude: ['../init'],
          include: [
            'login'
          ]
        }, {
          name: '../dashboard-page',
          exclude: ['../init'],
          include: [
            'dashboard',
            'models/gadget',
            'models/timer'
          ]
        }, {
          name: 'gadgets/agent-state/scripts/agent-state',
          exclude: [
            '../init',
            '../dashboard-page'
          ]
        }, {
          name: 'gadgets/session-manager/scripts/session-manager',
          exclude: [
            '../init',
            '../dashboard-page'
          ]
        }, {
          name: 'gadgets/agent-manager/scripts/agent-manager',
          exclude: [
            '../init',
            '../dashboard-page'
          ]
        }, {
          name: 'gadgets/my-statistics/scripts/my-statistics',
          exclude: [
            '../init',
            '../dashboard-page'
          ]
        }, {
          name: 'gadgets/outbound-call/scripts/outbound-call',
          exclude: [
            '../init',
            '../dashboard-page'
          ]
        }, {
          name: 'gadgets/queue-manager/scripts/queue-manager',
          exclude: [
            '../init',
            '../dashboard-page'
          ]
        }, {
          name: 'gadgets/call-recording/scripts/call-recording',
          exclude: [
            '../init',
            '../dashboard-page'
          ]
        }]
      },
      dev: {
        options: {
          optimize: 'none',
          optimizeCss: 'none',
          dir: '<%= settings.dev %>'
        }
      },
      dist: {
        options: {
          optimize: 'uglify',
          optimizeCss: 'standard',
          done: function (done, output) {
            grunt.task.run('copy:requirejs');
            done();
          }
        }
      }
    },

    copy: {
      dev: {
        expand: true,
        dot: true,
        cwd: '<%= settings.app %>',
        dest: '<%= settings.dev %>',
        src: '**'
      },
      dist: {
        files: [{
          expand: true,
          dot: true,
          cwd: '<%= settings.app %>',
          dest: '<%= settings.dist %>',
          src: [
            'fonts/*/fonts/*.{eot,svg,ttf,woff}',
            'images/{,*/}*.{gif,png}',
            'gadgets/*/images/*.{gif,png}'
          ]
        }]
      },
      requirejs: { // callback for requirejs
        files: [{
          expand: true,
          dot: true,
          cwd: '.tmp',
          dest: '<%= settings.dist %>',
          src: [
            'styles/*.css',
            'scripts/**/*.js',
            'gadgets/*/scripts/*.js',
            'gadgets/*/styles/*.css'
          ]
        }]
      }
    },
  });

  grunt.registerTask('dev', [
    'clean:dev',
    'copy:dev',
    'less:dev',
    'watch'
  ]);

  grunt.registerTask('build', [
    'clean:dist',
    'requirejs:dist',
    'less:dist',
    'copy:dist'
  ]);
  grunt.loadNpmTasks('grunt-newer');

  grunt.registerTask('default', [
    'newer:build'
  ]);
};
