#! /usr/bin/env python

import sys
import json

from ygp.parser import loads

def main(s):
    return json.dumps(loads(s))

if __name__ == '__main__':
    sys.stdout.write(main(sys.stdin.read()))
