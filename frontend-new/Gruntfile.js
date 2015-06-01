module.exports = function(grunt) {

	"use strict";

	grunt.initConfig({

		srcFiles:  ["src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"],
		pscMake: {
			all: {
				src: "<%=srcFiles%>"
			}
		},
		copy: [
{
expand: true,
cwd: "output",
src: ["**"],
dest: "tmp/node_modules/"
}, 
	{
		src: ["js/index.js"],
		dest: "tmp/index.js"
	}
	],

		psc: {
			options: {
				main: "S",
		modules: ["S"]
			},
		all: {
			src: ["<%=srcFiles%>"],
			dest: "dist/Main.js"
		}
		},
		dotPsci: ["<%=srcFiles%>"] ,
		watch: {
			files: [ "src/**/*.purs"
				, "bower_components/**/src/**/*.purs"],
			tasks: ["psc:all"]
		},
		browserify: {
			all: {
				src: ["tmp/index.js"],
				dest: "dist/index.js"
			}
		}
	});

	grunt.loadNpmTasks("grunt-contrib-copy");
	grunt.loadNpmTasks("grunt-purescript");
	grunt.loadNpmTasks('grunt-contrib-watch');
	grunt.loadNpmTasks('grunt-browserify');
//"psc:all", 
	grunt.registerTask("default", ["dotPsci","pscMake:all","copy", "browserify:all"]);
};
