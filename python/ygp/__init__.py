from __future__ import absolute_import
from __future__ import print_function

from ygp import clib
from ygp import parser
from ygp.parser import YGPError, YGPValueError
__all__ = ['load', 'loads', 'YGPError', 'YGPValueError']


def loads(string):
    return parser.loads(string, clib)


def load(file_obj):
    return parser.load(file_obj, clib)
