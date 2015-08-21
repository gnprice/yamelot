
SETUP
-----

* Follow the setup instructions in the root.  In particular create a
  virtualenv and activate it.

* Install libygp (from ../libygp).

* Install libffi.  E.g., `sudo apt-get install libffi-dev` if on
  Debian or Ubuntu.

* Run

```sh
pip install -e .
python setup.py develop
```

TESTS
-----

See README.md in root.

Also run tox here to test on all supported python/cffi combinations.


BENCHMARKS
----------
Here are some synthetic benchmarks results.

    [ygp]> python profile.py
    ygp int 2.18233709335
    yaml int 3.44159460068
    ygp str 1.52225019932
    yaml str 2.80367839336
    ygp list_map 2.69700028896
    yaml list_map 2.8210750103


    [ygp-pypy]> python profile.py
    ygp int 0.385481905937
    yaml int 2.48929121494
    ygp str 0.281080985069
    yaml str 2.08205859661
    ygp list_map 0.348178219795
    yaml list_map 2.76920659542

