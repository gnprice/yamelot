yamelot
=======



Language definition
-------------------

The tl;dr: Yamelot is a subset (*) of YAML.  Yamelot consists of YAML
without tags (`!!foo`), and without some obscure and rarely-used
features (LINK differences).

For a self-contained definition, see (LINK spec).

(*) modulo bugs!  The spec is early and bugs are likely.


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


