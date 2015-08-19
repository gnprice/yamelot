var fs = require('fs');
var indentParser = require('./indent.js');

process.argv.slice(2).forEach(function (fileName) {
    var text = fs.readFileSync(fileName, "utf8");
    var indentedText = indentParser.parse(text);
    console.log('============== Indented')
    console.log(indentedText)
});
