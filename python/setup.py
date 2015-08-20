import sys

from setuptools import setup, find_packages

try:
    import _cffi_backend
    installed_cffi = _cffi_backend.__version__
except ImportError:
    installed_cffi = None

if '_cffi_backend' in sys.builtin_module_names:   # PyPy
    import _cffi_backend
    requires_cffi = 'cffi==' + _cffi_backend.__version__
elif installed_cffi is not None:
    requires_cffi = 'cffi==' + installed_cffi
else:
    requires_cffi = 'cffi>=1.0.0'


if requires_cffi.startswith('cffi==0.'):
    # backward compatibility: we have 'cffi==0.*'
    from ygp.clib.ygp_build import ffi
    extra_args = dict(
        ext_modules=[ffi.verifier.get_extension()],
    )
else:
    extra_args = dict(
        setup_requires=[requires_cffi],
        cffi_modules=['ygp/clib/ygp_build.py:ffi'],
    )

setup(
    name='ygp',
    version='0.1',
    packages=find_packages(exclude=['cffi_build']),
    install_requires=requires_cffi,
    zip_safe=False,
    **extra_args
)
