/*
 * Comments
 */

rest_of_line_maybe_comments  // YAML 1.2 [79] s-l-comments
  = (nonbreak_whitespace / empty_rest_of_line)+
    first:comment
    rest:empty_or_comment_line*
    { return new Comment((first + '\n' + rest.join('')).trim()) }
  / empty_rest_of_line empty_line+ {}

empty_or_comment_line
  = $(empty_line)
  / line_start s:$(nonbreak_whitespace* comment) { return s }

comment
  = "#" nonbreak_whitespace* text:$(nonbreak*) empty_rest_of_line { return text || "" }

/*
 * Line and file primitives
 */

empty_rest_of_line = $(nonbreak_whitespace* line_break_or_eof)
empty_line = line_start empty_rest_of_line  // YAML 1.2 [70]

dent_tok = ( indent_tok / dedent_tok / badent_tok )
indent_tok = line_start "@@INDENT@@" {}
dedent_tok = line_start "@@DEDENT@@" {}
badent_tok = line_start "@@BADENT@@" {}

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

nonbreak_nonspace = "\t" / [\u0021-\u007e]
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
