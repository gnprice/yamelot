from __future__ import absolute_import
from __future__ import print_function

import datetime
import json

import pytest

import ygp


def default(obj):
    if isinstance(obj, datetime.datetime):
        return 'datetime({}, {}, {}, {}, {}, {}, {})'.format(
            obj.year,
            obj.month,
            obj.day,
            obj.hour,
            obj.minute,
            obj.second,
            obj.microsecond,
        )
    elif isinstance(obj, datetime.date):
        return 'date({}, {}, {})'.format(
            obj.year,
            obj.month,
            obj.day,
        )
    raise TypeError


class IntergrationTestFile(pytest.File):
    def collect(self):
        test_input = self.fspath.read()

        should_xfail = test_input.startswith('XFAIL\n')
        if should_xfail:
            self.add_marker(pytest.mark.xfail)
            _, test_input = test_input.split('\n', 1)

        if '!' * 80 in test_input:
            input_ygp, _ = test_input.split('!' * 80, 1)
            return [ExpectedFailingTestItem(
                name=self.fspath.basename,
                parent=self,
                ygp=input_ygp,
            )]
        else:
            input_ygp, input_json = test_input.split('=' * 80)
            return [IntergrationTestItem(
                name=self.fspath.basename,
                parent=self,
                ygp=input_ygp,
                json=input_json,
            )]


class ExpectedFailingTestItem(pytest.Item):
    def __init__(self, name, parent, ygp):
        super(ExpectedFailingTestItem, self).__init__(name, parent)
        self._ygp = ygp
        self.obj = lambda: 'a'  # Fake for xfail marks

    def reportinfo(self):
        return self.fspath, self.name, '{}:{}'.format(self.fspath, self.name)

    def runtest(self):
        try:
            parsed_ygp = ygp.loads(self._ygp)
        except ValueError:
            pass
        else:
            raise UnexpectedSuccessError(parsed_ygp)

    def repr_failure(self, excinfo):
        value = excinfo.value
        if isinstance(value, UnexpectedSuccessError):
            return 'Test should have failed but parsed to\n{}'.format(
                value.parsed_ygp
            )
        return super(ExpectedFailingTestItem, self).repr_failure(excinfo)


class IntergrationTestItem(pytest.Item):
    def __init__(self, name, parent, ygp, json):
        super(IntergrationTestItem, self).__init__(name, parent)
        self._ygp = ygp
        self._json = json
        self.obj = lambda: 'a'  # Fake for xfail marks

    def runtest(self):
        try:
            parsed_ygp = ygp.loads(self._ygp)
            json_ygp = json.dumps(parsed_ygp, default=default)
            normalizd_ygp = json.loads(json_ygp)
        except ValueError as e:
            raise YGPParseError(self._ygp, e)

        try:
            parsed_json = json.loads(self._json)
        except ValueError as e:
            raise JSONParseError(self._json, e)

        if normalizd_ygp != parsed_json:
            raise IntergrationTestError(normalizd_ygp, parsed_json)

    def repr_failure(self, excinfo):
        value = excinfo.value
        if isinstance(value, JSONParseError):
            return 'JSON format error: {}\n{}'.format(
                value.error, value.json_input
            )
        if isinstance(value, YGPParseError):
            return 'YGP format error: {}\n{}'.format(
                value.error, value.ygp_input
            )
        elif isinstance(value, IntergrationTestError):
            return IntergrationTestFailureRepr(self.config, value)
        return super(IntergrationTestItem, self).repr_failure(excinfo)

    def reportinfo(self):
        return self.fspath, self.name, '{}:{}'.format(self.fspath, self.name)


class IntergrationTestError(Exception):
    def __init__(self, parsed_ygp, parsed_json):
        self.parsed_ygp = parsed_ygp
        self.parsed_json = parsed_json


class UnexpectedSuccessError(Exception):
    def __init__(self, parsed_ygp):
        self.parsed_ygp = parsed_ygp


class JSONParseError(Exception):
    def __init__(self, json_input, error):
        self.json_input = json_input
        self.error = error


class YGPParseError(Exception):
    def __init__(self, ygp_input, error):
        self.ygp_input = ygp_input
        self.error = error


class IntergrationTestFailureRepr(object):
    def __init__(self, config, value):
        self.value = value
        self.config = config
        assertion_plugin = self.config.pluginmanager.getplugin('assertion')
        self.assertrepr_compare = assertion_plugin.pytest_assertrepr_compare

    def __str__(self):
        exp = self.assertrepr_compare(
            self.config,
            '==',
            self.value.parsed_ygp,
            self.value.parsed_json,
        )
        return '  \n'.join(exp)

    def toterminal(self, tw):
        lines = self.assertrepr_compare(
            self.config,
            '==',
            self.value.parsed_ygp,
            self.value.parsed_json,
        )
        for line in lines:
            tw.line('    ' + line)


def pytest_collect_file(parent, path):
    if path.ext == ".test":
        return IntergrationTestFile(path, parent)
