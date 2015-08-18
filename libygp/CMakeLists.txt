# Minimal CMake project for building a static library under Windows.

cmake_minimum_required (VERSION 2.8)
project (ygp C)

set (YGP_VERSION_MAJOR 0)
set (YGP_VERSION_MINOR 1)
set (YGP_VERSION_PATCH 6)
set (YGP_VERSION_STRING "${YGP_VERSION_MAJOR}.${YGP_VERSION_MINOR}.${YGP_VERSION_PATCH}")

file (GLOB SRC src/*.c)

include_directories (include win32)
add_definitions (-DHAVE_CONFIG_H -DYGP_DECLARE_STATIC)
add_library (ygp STATIC ${SRC})

