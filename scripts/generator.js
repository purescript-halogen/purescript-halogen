/**
* 1) target files must exists 
* 2) target files must have imported ClassName and className
* 3) you should check files after generating for incorrect class names (i.e com)
* 4) you should rename some clashes like ```show``` or ```in```
**/

var fs = require("fs"),
    cc = require("change-case").camelCase,
    _ = require("lodash");

function generate(source, output) {
    var content = fs.readFileSync(source, {encoding: "utf-8"}),
        comments = /(?:\/\*(?:[\s\S]*?)\*\/)|(?:([\s;])+\/\/(?:.*)$)/gm,
        re = /\.[a-z\-][a-zA-Z0-9\-_]+/g,
        matches = _.uniq(content.replace(comments, "").match(re)).sort(),
        result = "";
    matches.forEach(function(cl) {
        var snake = cl.replace(".", ""),
            camel = cc(snake);
        result += "\n";
        result += camel + " :: ClassName\n";
        result += camel + " = className \"" + snake + "\"\n";
    });
    fs.appendFileSync(output, result, {encoding: "utf-8"});
}


function bootstrap() {
    generate(
        "bower_components/bootstrap/dist/css/bootstrap.min.css",
        "src/Halogen/Themes/Bootstrap3.purs"
    );
}
function foundation() {
    generate(
        "bower_components/foundation/css/foundation.css",
        "src/Halogen/Themes/Foundation5.purs"
    );
}
bootstrap();
foundation();

