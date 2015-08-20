var fs = require('fs');
var yamlParser = require('./grammar.js');

process.argv.slice(2).forEach(function (fileName) {
    var text = fs.readFileSync(fileName, "utf8");
    var parsedOutput = yamlParser.parse(text);
    console.log('============== parsedOutput')
    console.log(parsedOutput)
});
