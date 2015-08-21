import timeit

try:
    from yaml import CSafeLoader
    yaml_init = 'import yaml; from yaml import CSafeLoader'
    yaml_load = 'yaml.load(open("%s"), Loader=CSafeLoader)'
except ImportError:
    yaml_init = 'import yaml'
    yaml_load = 'yaml.load(open("%s"))'


n = 10
print 'ygp int', timeit.timeit('ygp.load(open("big_int.yaml"))', 'import ygp', number=n)/n
print 'yaml int', timeit.timeit(yaml_load % 'big_int.yaml', yaml_init, number=n)/n

print 'ygp str', timeit.timeit('ygp.load(open("big_str.yaml"))', 'import ygp', number=n)/n
print 'yaml str', timeit.timeit(yaml_load % 'big_str.yaml', yaml_init, number=n)/n

print 'ygp list_map', timeit.timeit('ygp.load(open("big_list_map.yaml"))', 'import ygp', number=n)/n
print 'yaml list_map', timeit.timeit(yaml_load % 'big_list_map.yaml', yaml_init, number=n)/n

#print 'yaml', timeit.timeit('yaml.load(open("big.yaml"))', 'import yaml', number=n)/n
