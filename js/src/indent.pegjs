{
  function assemble(first, rest, last) {
    var text_pieces = [first];
	for (var i = 0; i < rest.length; i++) {
	  text_pieces = text_pieces.concat(rest[i]);
	}
	if (last) {
	  text_pieces.unshift(last);
	}
	return text_pieces.join("");
  }

  var depths = [0];

  function indent(s) {
    var depth = s.length;
    
    if (depth == depths[0]) return [];
    
    if (depth > depths[0]) {
      depths.unshift(depth);
      return ["@@INDENT@@"];
    }
    
    var dedents = [];
    while (depth < depths[0]) {
      depths.shift();
      dedents.push("@@DEDENT@@");
    }
    
    if (depth != depths[0]) {
	  dedents.push("@@BADDENT@@");
	}
    
    return dedents;
  }
}

start
  = first:line
    rest:(line_break line)*
	last:line_break?           { return assemble(first, rest, last) }

line
  = s:$(empty_or_comment_line) { return s }
  / depth:line_indent
    s:$(nonbreak_nonspace nonbreak*)
	                           { return depth[0].concat([depth[1] + s]).join("") }

line_indent = s:$(" "*)         { return [indent(s), s] }
