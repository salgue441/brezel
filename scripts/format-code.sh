#!/usr/bin/env bash
# scripts/format-code.sh
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo "Formatting code"
find "${PROJECT_ROOT}/include" "${PROJECT_ROOT}/src" "${PROJECT_ROOT}/tests" -name "*.hpp" -o -name "*.cpp" | xargs clang-format -i

echo "Code formatting complete!"
