{
  function apply_or_error(func, value) {
    if (value instanceof Error)
	  return value;
	else
	  return func(value);
  }

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
  = value:scalar { return new Node(value) }
  / rest_of_line_maybe_comments
  / empty_line
  / file_end

directive  // YAML 1.2 [[82]
  = $( "%YAML" nonbreak* rest_of_line_maybe_comments
     / "%TAG" nonbreak* rest_of_line_maybe_comments
     / "%.+" nonbreak* rest_of_line_maybe_comments
	 )

/*
 * Scalars
 */

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

/*
 * Line breaks
 */

folded_line_break  // YAML 1.2 [73]
  = line_break empty_lines:empty_line+ { return repeated('\n', empty_lines.length).join("") }
  / line_break { return " " }
