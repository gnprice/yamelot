from setuptools import setup, find_packages

setup(
    name='ygp',
    version='0.1',
    packages=find_packages(exclude=['cffi_build']),
    setup_requires=['cffi>=1.0.0'],
    install_requires=['cffi>=1.0.0'],
    cffi_modules=[
        'cffi_build/yaml_build.py:ffi',
        'cffi_build/ygp_build.py:ffi',
    ],
)
