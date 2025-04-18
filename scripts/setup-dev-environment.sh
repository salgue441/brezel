#!/usr/bin/env bash
set -euo pipefail

# Script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Default settings
CMAKE_PRESET="dev"

echo "Setting up Brezel development environment"

# Check for required tools
echo "Checking prerequisites"
for cmd in cmake git ninja; do
  if ! command -v "$cmd" &>/dev/null; then
    echo "Error: $cmd is not installed. Please install it and try again."
    exit 1
  fi
done

# Setup vcpkg
echo "Setting up vcpkg"
"${SCRIPT_DIR}/setup-vcpkg.sh"

# Source the environment
if [ -f "${PROJECT_ROOT}/.env" ]; then
  source "${PROJECT_ROOT}/.env"
fi

# Verify VCPKG_ROOT is set
if [ -z "${VCPKG_ROOT:-}" ]; then
  echo "Error: VCPKG_ROOT is not set. Please run setup-vcpkg.sh first."
  exit 1
fi

# Configure the project
echo "Configuring project with preset: $CMAKE_PRESET"
cmake --preset="$CMAKE_PRESET"

echo "Development environment setup complete!"
echo "You can now build the project with: cmake --build --preset=$CMAKE_PRESET"
