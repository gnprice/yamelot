{
  function Node(value) {
    this.value = value;
  }

  function Document(node) {
    this.node = node;
  }
}

start
  = stream

stream
  = node
  / document+

document
  = "---" whitespace node:node whitespace ("..."?) { return new Document(node) }

node "node"
  = value:scalar { return new Node(value) }

// node "node"
//   = scalar / map / sequence
//
// map "map"
//   = whitespace_map
//   / inline_map
// 
// whitespace_map
//   = map_key ":" map_value:node (("\n" whitespace_map)?)

whitespace
  = "\n" / " " / "\t"

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

bom
  = "\ufeff"

indicator
  = [-?:,|{}#&*!|>'"%@`] / "[" / "]"

flow_indicator = [,{}] / "[" / "]"

empty_line = whitespace* line_break

line_break = "\n" / "\r" / "\r\n"

printable
  = "\t"
  / "\n"
  / "\r"
  / [\u0020-\u007e]

nonbreak = "\t" / [\u0020-\u007e]
nonspace = [\u0021-\u007e] // space is \u0020
whitespace = " " / "\t"
reserved = "@" | "`"

escaped_char = "\\" (
  / [0abtnvfre"/N_LP]
  / "\u00x9"  // literal tab
  / "\u0020"  // literal space
  / "\\"      // literal backslash
  / "x" hex_digit hex_digit
  / "u" hex_digit hex_digit hex_digit hex_digit
  / "U" hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit

hex_digit = [0-9a-fA-F]
