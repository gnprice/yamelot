import json
import shlex
import subprocess

import py
import pytest


def call_with_input(executable, stdin_data):
    sub = subprocess.Popen(
        shlex.split(executable),
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE
    )
    stdoutdata, stderrdata = sub.communicate(stdin_data)
    sub.wait()
    if sub.returncode != 0:
        raise SubprocessError()
    return stdoutdata


class IntegrationTestFile(pytest.File):
    def __init__(self, path, parent, runners):
        super(IntegrationTestFile, self).__init__(path, parent)
        self._runners = runners

    def collect(self):
        test_input = self.fspath.read()
        if '!' * 80 in test_input:
            input_ygp, _ = test_input.split('!' * 80, 1)
            return [
                ExpectedFailingTestItem(
                    name=str(runner),
                    parent=self,
                    ygp=input_ygp,
                    executable=str(runner),
                )
                for runner in runners
            ]
        else:
            input_ygp, input_expected = test_input.split('=' * 80)
            return [
                IntegrationTestItem(
                    name=str(runner),
                    parent=self,
                    ygp=input_ygp,
                    expected=input_expected,
                    executable=str(runner),
                )
                for runner in runners
            ]


class ExpectedFailingTestItem(pytest.Item):
    def __init__(self, name, parent, ygp, executable):
        super(ExpectedFailingTestItem, self).__init__(name, parent)
        self._ygp = ygp
        self._executable = executable

    def runtest(self):
        try:
            call_with_input(self._executable, self._ygp)
        except SubprocessError:
            pass
        else:
            raise UnexpectedSuccessError(self._ygp)

    def repr_failure(self, excinfo):
        value = excinfo.value
        if isinstance(value, UnexpectedSuccessError):
            return 'Test should have failed but parsed to\n{}'.format(
                value.parsed_ygp
            )
        return super(ExpectedFailingTestItem, self).repr_failure(excinfo)

    def reportinfo(self):
        return self.fspath, self.name, '{}:{}'.format(self.fspath, self.name)


class IntegrationTestItem(pytest.Item):
    def __init__(self, name, parent, ygp, expected, executable):
        super(IntegrationTestItem, self).__init__(name, parent)
        self._ygp = ygp
        self._expected = expected
        self._executable = executable

    def runtest(self):
        try:
            output = call_with_input(self._executable, self._ygp)
        except subprocess.CalledProcessError as e:
            raise YGPParseError(self._ygp, e)

        output_tree = test_output_to_tree(output)
        expected_tree = test_output_to_tree(self._expected)

        if output_tree != expected_tree:
            raise IntegrationTestError(output_tree, expected_tree)

    def repr_failure(self, excinfo):
        value = excinfo.value
        if isinstance(value, TestTextParseError):
            return 'TestTextParseError format error: {}\n{}'.format(
                value.error, value.expected
            )
        if isinstance(value, YGPParseError):
            return 'YGP format error: {}\n{}'.format(
                value.error, value.ygp_input
            )
        elif isinstance(value, IntegrationTestError):
            return IntegrationTestFailureRepr(self.config, value)
        return super(IntegrationTestItem, self).repr_failure(excinfo)

    def reportinfo(self):
        return self.fspath, self.name, '{}:{}'.format(self.fspath, self.name)


def test_output_to_tree(test_output):
    try:
        return json.loads(test_output)
    except ValueError as e:
        raise TestTextParseError(test_output, e)


class SubprocessError(Exception):
    pass


class IntegrationTestError(Exception):
    def __init__(self, output_tree, expected_tree):
        super(UnexpectedSuccessError, self).__init__(
            output_tree,
            expected_tree,
        )
        self.output_tree = output_tree
        self.expected_tree = expected_tree


class UnexpectedSuccessError(Exception):
    def __init__(self, parsed_ygp):
        super(UnexpectedSuccessError, self).__init__(parsed_ygp)
        self.parsed_ygp = parsed_ygp


class TestTextParseError(Exception):
    def __init__(self, test_text, error):
        super(TestTextParseError, self).__init__(test_text, error)
        self.test_text = test_text
        self.error = error


class YGPParseError(Exception):
    def __init__(self, ygp_input, error):
        self.ygp_input = ygp_input
        self.error = error


class IntegrationTestFailureRepr(object):
    def __init__(self, config, value):
        self.value = value
        self.config = config
        assertion_plugin = self.config.pluginmanager.getplugin('assertion')
        self.assertrepr_compare = assertion_plugin.pytest_assertrepr_compare

    def __str__(self):
        exp = self.assertrepr_compare(
            self.config,
            '==',
            self.value.output_tree,
            self.value.expected_tree,
        )
        return '  \n'.join(exp)

    def toterminal(self, tw):
        lines = self.assertrepr_compare(
            self.config,
            '==',
            self.value.output_tree,
            self.value.expected_tree,
        )
        for line in lines:
            tw.line('    ' + line)


runners = set()


def pytest_configure(config):
    global runners
    for p in config.option.file_or_dir:
        path = py.path.local(p)
        runners |= set(str(irp) for irp in path.visit('integration_runner'))


def pytest_collect_file(parent, path):
    if path.ext == ".test":
        return IntegrationTestFile(path, parent, runners)
