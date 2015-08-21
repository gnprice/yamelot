

...

A token stream t "can be completed by" a stream s if the concatenation
of t and s has a valid parse.

When scanning for a token, we are "in block mode" if the stream either
can be completed by the stream `[SCALAR]` or can be completed by the
empty stream.  We are "in flow-key mode" if the stream can be
completed by the stream `[RIGHT-BRACE]`.  Otherwise we are "in flow
mode".

PLAIN_SCALAR Block = ...
PLAIN_SCALAR FlowKey = ...
PLAIN_SCALAR Flow = ...

