#!/usr/bin/env bash
set -euo pipefail

# Script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Default settings
CMAKE_PRESET="dev"

# Parse arguments
for arg in "$@"; do
  case $arg in
  --preset=*)
    CMAKE_PRESET="${arg#*=}"
    shift
    ;;
  *)
    # Unknown option
    ;;
  esac
done

# Source the environment
if [ -f "${PROJECT_ROOT}/.env" ]; then
  source "${PROJECT_ROOT}/.env"
fi

# Build the project
echo "Building with preset: $CMAKE_PRESET"
cmake --build --preset="$CMAKE_PRESET"

echo "Build complete!"
