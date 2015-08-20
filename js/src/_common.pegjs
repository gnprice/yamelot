/*
 * Anchor and Alias
 */

alias_node "alias"  // YAML 1.2 [104]
  = "*" name:$(anchor_char+) { return new Token("alias", name) }

anchor_property "anchor" // YAML 1.2 [101]
  = "&" name:$(anchor_char+) { return new Token("anchor", name) }

anchor_char = !flow_indicator c:nonbreak_nonspace { return c }

/*
 * Directive
 */

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
 * Plain-style scalar
 *
 * There are two contexts here:
 *
 * - Plain scalar within a flow context; flow indicators forbidden
 *   "plain_scalar_in_flow"
 * - Plain sclar outside of a flow context; flow indicators allowed
 *   except for initial character; "plain_scalar"
 */

// YAML 1.2 [133] ns-plain-one-line
plain_scalar "plain-style scalar"
  = v:$(plain_first plain_rest_char*) { return v.trim() }

plain_first
  = $(!indicator nonwhite "#"*)
  / $([?:-] nonwhite "#"*)

plain_rest_char
  = $(nonwhite "#")
  / $(":" nonwhite)
  / $(![:#] nonbreak)

// YAML 1.2 [133] ns-plain-one-line
plain_scalar_in_flow "plain-style scalar in flow context"
  = v:$(plain_first_in_flow plain_rest_in_flow_char*) { return v.trim() }

plain_first_in_flow
  = $(!indicator nonwhite_in_flow "#"*)
  / $([?:-] nonwhite_in_flow "#"*)

plain_rest_in_flow_char
  = $(nonwhite_in_flow "#")
  / $(":" nonwhite_in_flow)
  / $(![:#] !flow_indicator nonbreak)

nonwhite_in_flow = !flow_indicator nonwhite

/*
 * Single-quoted strings
 */

singlequote_string
  = "'" c:singlequote_content_char* "'" { return c.join("") }

singlequote_content_char
  = "''"             { return "'" }
  / !"'" c:nonbreak  { return c }

/*
 * One-line double-quoted strings
 */

oneline_doublequote_string
  = "\"" c:oneline_doublequote_content_char* "\"" { return c.join("") }

oneline_doublequote_content_char
  = escaped_char / $(![\\"] nonbreak)

/*
 * Multiline Double-quoted strings
 */

multiline_doublequote_string
  = "\"" c:multiline_doublequote_content_char* "\"" { return c.join("") }

multiline_doublequote_content_char
  = escaped_char
  / s:nonbreak_whitespace* "\\" line_break { return s.join("") }
  / flow_folded_space
  / $(![\\"] nonbreak)

/*
 * Foldable whitespace
 */

flow_folded_space  // YAML 1.2 [74] b-l-folded
  = nonbreak_whitespace* line_break l:(nonbreak_whitespace* line_break)+
                                    { return repeated("\n", l.length).join("") }
  / nonbreak_whitespace* line_break { return " "}
  / line_start nonbreak_whitespace+ { return "" }

/*
 * Comments
 */

rest_of_line_maybe_comments  // YAML 1.2 [79] s-l-comments
  = (nonbreak_whitespace / line_break)+ first:comment
    rest:line_break_empty_or_comment*
    { return new Comment((first + rest.join('')).trim()) }
  / (nonbreak_whitespace / line_break)* {}

line_break_empty_or_comment
  = line_break w:nonbreak_whitespace*  { return "\n" }
  / line_break c:comment               { return "\n" + c }

comment
  = "#" nonbreak_whitespace* c:nonbreak* { return c.join("") }

/*
 * Line and file primitives
 */

empty_rest_of_line = w:nonbreak_whitespace*

dent_tok = ( indent_tok / dedent_tok / badent_tok )
indent_tok = line_start "@@INDENT@@" {}
dedent_tok = line_start "@@DEDENT@@" {}
badent_tok = line_start "@@BADENT@@" {}

line_start = &. &{ return column() == 1 }
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

nonwhite = [\u0021-\u007e]
nonbreak_nonspace = "\t" / [\u0021-\u007e]
nonbreak = "\t" / [\u0020-\u007e]  // YAML 1.2 [27]
nonspace = [\u0021-\u007e] // space is \u0020; YAML 1.2 [34]
nonbreak_whitespace = " " / "\t"  // YAML 1.2 [33]
reserved = "@" / "`"  // YAML 1.2 [21]

escaped_char =  // YAML 1.2 [62]
  $ "\\"
   ( [0abefnrtvNLP"/_\t \\]
   / "x" hex_digit hex_digit
   / "u" hex_digit hex_digit hex_digit hex_digit
   / "U" hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
   )

hex_digit = [0-9a-fA-F]  // YAML 1.2 [36]
word_char = [0-9a-zA-Z] / "-"  // YAML 1.2 [38]
