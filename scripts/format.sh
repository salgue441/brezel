#!/bin/bash
# format-code.sh

find include -name "*.hpp" -o -name "*.cpp" | xargs clang-format -i
