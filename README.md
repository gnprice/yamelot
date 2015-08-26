yamelot
=======


testing & validation
--------------------

Yamelot includes test specs to be used to validate all implementations.
A general purpose test framework is written in python;
each implementation plugs in via a simple stdin/stdout interface.
Implementations are plugged in via a `python/integration_runner.{which}` executable file.

The following steps should prepare the validation framework to run.
(Each implementation may have its own build and dep process;
these should be covered in their individual READMEs.)

```bash
# clone!
git clone https://github.com/gnprice/yamelot.git
cd yamelot

# get virtualenv
# for linux, this is:
apt-get install virtualenv

# whip up a clean env
virtualenv env
. ./env/bin/activate
pip install pytest

# run tests!
# drop the "-k {thingy}" part to run all impls.
py.test tests -k libyaml
```

`libyaml` in this example may be replaced by any other suffix of the `integration_runner` files.



Language definition
-------------------

Yamelot is a subset of YAML.  See the [YAML spec](http://yaml.org/spec/1.2/spec.html)
for details of YAML syntax. Yamelot differs from YAML in the following ways:

* No tags!  This means no referring to arbitrary application data
  types, which means no executing arbitrary code in loading a Yamelot
  document.
* UTF-8 always
* No BOM
* No schemas
* No cycles
* Anchors must be globally unique within a document, scoped to document
* No octal (give an error)
* No binary
* No folded scalars (`>`)
* No chomping indicators (`-`/`+`)
* No `~` for `null`, no empty nodes (except empty nodes as mapping values)
* No explicit keys (`?`)
* No "default" mapping values (`=`)
* No multiline bare scalars
* No y/Y/n/N booleans
* No base-60 numbers

We contemplate a `gofmt`-like tool for canonicalizing the style of
Yamelot documents.  The following YAML features will likely be
canonicalized away by such a tool, though they remain part of Yamelot:

* Single-quoted strings
* Directives
* Empty nodes as mapping values (canonicalize to explicit `null`)

Yamelot retains all other features of YAML unchanged.  In particular
this includes the following:

* Anchor, alias, merge
* In-order reference resolution (anchor must appear before reference)
* Spaces-only for indentation
* Indentation annotation for literal blocks
* Arbitrarily large integers
* [Timestamps](http://yaml.org/type/timestamp.html)
* Streams and document start/end indicators (`---`, `...`)
* The indicator characters `@` and "`" are reserved
