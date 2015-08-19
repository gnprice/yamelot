{
  function Node(value) {
    this.value = value;
  }

  function Comment(value) {
    this.text = value;
  }

  function Document(node) {
    this.node = node;
  }

  function repeated(char, times) {
    var _array = [];
	var i;
	for (i = 0; i < times; i++) {
	  _array.push(char);
	}
	return _array;
  }
}

start
  = stream

stream
  = node

node "node"
  = empty_line
  / rest_of_line_maybe_comments
  / value:scalar { return new Node(value) }

scalar
  = integer
  / float
  / boolean
  / null

integer "integer"
  = decimal_integer / hex_integer

decimal_integer
  = (!"0x") (value:$("-"? [1-9][0-9]*)) (!".") { return parseInt(value, 10) }

hex_integer
  = "0x" value:$(hex_digit+) { return parseInt(value, 16) }

float "float"
  = infinity { return Number.POSITIVE_INFINITY }
  / "-" infinity { return Number.NEGATIVE_INFINITY }
  / (".nan" / ".NaN" / ".NAN") { return Number.NaN }
  / value:$([-+]? [0-9]+ "e"i [-+]? [0-9]+ ) { return parseFloat(value) }
  / value:$([-+]? ("." [0-9]+ / [0-9]+ ("." [0-9]*)?) ( "e"i [-+]? [0-9]+ )?) { return parseFloat(value) }

infinity
  = ".inf" / ".Inf" / ".INF"

boolean "boolean"
  = ("true" / "True" / "TRUE") { return true }
  / ("false" / "False" / "FALSE") { return false }

null "null"
  = ("null" / "Null" / "NULL") { return null }

directive  // YAML 1.2 [[82]
  = $( "%YAML" nonbreak* rest_of_line_maybe_comments
     / "%TAG" nonbreak* rest_of_line_maybe_comments
     / "%.+" nonbreak* rest_of_line_maybe_comments
	 )

/*
 * Comments
 */

rest_of_line_maybe_comments  // YAML 1.2 [79] s-l-comments
  = (nonbreak_whitespace / empty_rest_of_line)+
    first:comment
    rest:empty_or_comment_line*
    { return new Comment(first + rest.join('')) }
  / empty_rest_of_line empty_line+ { return undefined }

empty_or_comment_line
  = empty_line { return '' }
  / line_start nonbreak_whitespace* value:comment { return value }

comment
  = "#" nonbreak_whitespace* text:$(nonbreak*) empty_rest_of_line { return text || "" }

/*
 * Line breaks
 */

folded_line_break  // YAML 1.2 [73]
  = line_break empty_lines:empty_line+ { return repeated('\n', empty_lines.length).join("") }
  / line_break { return " " }

/*
 * Line and file primitives
 */

empty_rest_of_line = nonbreak_whitespace* line_break_or_eof
empty_line = line_start empty_rest_of_line  // YAML 1.2 [70]

line_break_or_eof = line_break / file_end
line_start = &{ return column() == 1 }
line_end = &line_break / file_end
file_start = &{ return offset() == 0 }
file_end = !.

/*
 * Character classes
 */

bom = "\ufeff"

line_break = "\n" / "\r" / "\r\n"

indicator = [-?:,|{}#&*!|>'"%@`] / "[" / "]"  // YAML 1.2 [22]

flow_indicator = [,{}] / "[" / "]"  // YAML 1.2 [23]

printable  // YAML 1.2 [1]
  = "\t" / "\n" / "\r" / [\u0020-\u007e]

nonbreak = "\t" / [\u0020-\u007e]  // YAML 1.2 [27]
nonspace = [\u0021-\u007e] // space is \u0020; YAML 1.2 [34]
nonbreak_whitespace = " " / "\t"  // YAML 1.2 [33]
reserved = "@" / "`"  // YAML 1.2 [21]

escaped_char =  // YAML 1.2 [62]
  "\\"
  (
  / [0abtnvfre"/N_LP]
  / "\t"  // literal tab
  / " "  // literal space
  / "\\"      // literal backslash
  / "x" hex_digit hex_digit
  / "u" hex_digit hex_digit hex_digit hex_digit
  / "U" hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
  )

hex_digit = [0-9a-fA-F]  // YAML 1.2 [36]
word_char = [0-9a-zA-Z] / "-"  // YAML 1.2 [38]
