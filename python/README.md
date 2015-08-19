
SETUP
-----

* Install libffi.  E.g., `sudo apt-get install libffi-dev` if on
  Debian or Ubuntu.

* Install libygp (from ../libygp).

* Use a virtualenv.

* Run
```
pip install -e .
python setup.py develop
pip install pytest
```

TESTS
-----

Run `py.test` in the parent directory (the root of the repo.)
