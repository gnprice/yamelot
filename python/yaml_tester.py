import sys

import pprint
import time
import yaml
import ygp

try:
    from yaml import CSafeLoader as SafeLoader
except:
    from yaml import SafeLoader

path = sys.argv[1]
ygp_time = 0
yaml_time = 0

for path in sys.argv[1:]:
    print 'checking', path

    src = open(path).read()

    start = time.time()
    yaml_version = yaml.load(src, Loader=SafeLoader)
    yaml_time += time.time() - start

    start = time.time()
    ygp_version = ygp.loads(src)
    ygp_time += time.time() - start

    assert ygp_version == yaml_version

print 'YGP:', ygp_time
print 'YAML:', yaml_time
