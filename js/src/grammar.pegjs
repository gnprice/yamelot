{
  function apply_or_error(func, value) {
    if (value instanceof Error)
	  return value;
	else
	  return func(value);
  }

  function Comment(value) {
    this.type = "comment";
    this.value = value;
  }

  function Node(value) {
    this.value = value;
  }

  function Token(type, value) {
    this.type = type;
    this.text = value;
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
  = v:bare_string_in_flow line_break?
    { return JSON.stringify(v) }
