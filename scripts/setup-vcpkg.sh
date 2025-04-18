#!/usr/bin/env bash
set -euo pipefail

# Script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Default settings
VCPKG_ROOT="${PROJECT_ROOT}/vcpkg"
VCPKG_COMMIT="a42af01b72c28a8e1d7b48107b33e4f286a55ef6"

# Colors for output
GREEN="\033[0;32m"
YELLOW="\033[0;33m"
RED="\033[0;31m"
RESET="\033[0m"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
  --vcpkg-root=*)
    VCPKG_ROOT="${1#*=}"
    shift
    ;;
  --vcpkg-commit=*)
    VCPKG_COMMIT="${1#*=}"
    shift
    ;;
  -h | --help)
    echo "Usage: $0 [options]"
    echo "Set up vcpkg for the Brezel project."
    echo ""
    echo "Options:"
    echo "  --vcpkg-root=PATH   Path to vcpkg installation (default: ./vcpkg)"
    echo "  --vcpkg-commit=SHA  Use specific vcpkg commit (default: recent stable)"
    echo "  -h, --help          Show this help message"
    exit 0
    ;;
  *)
    echo -e "${RED}Unknown option: $1${RESET}"
    exit 1
    ;;
  esac
done

echo -e "${GREEN}Setting up vcpkg in ${VCPKG_ROOT}...${RESET}"

# Clone vcpkg if it doesn't exist
if [ ! -d "${VCPKG_ROOT}" ]; then
  echo -e "${YELLOW}Cloning vcpkg${RESET}"
  git clone https://github.com/microsoft/vcpkg.git "${VCPKG_ROOT}"
else
  echo -e "${YELLOW}vcpkg directory already exists, updating${RESET}"
  git -C "${VCPKG_ROOT}" fetch
fi

# Checkout specific commit
echo -e "${YELLOW}Checking out vcpkg commit ${VCPKG_COMMIT}${RESET}"
git -C "${VCPKG_ROOT}" checkout "${VCPKG_COMMIT}"

# Bootstrap vcpkg
echo -e "${YELLOW}Bootstrapping vcpkg${RESET}"
"${VCPKG_ROOT}/bootstrap-vcpkg.sh" -disableMetrics

# Create .env file with vcpkg path
echo -e "${YELLOW}Creating .env file${RESET}"
echo "VCPKG_ROOT=${VCPKG_ROOT}" >"${PROJECT_ROOT}/.env"

# Set environment variable for the current session
export VCPKG_ROOT="${VCPKG_ROOT}"

echo -e "${GREEN}vcpkg setup complete!${RESET}"
echo ""
echo "You can now use vcpkg with CMake:"
echo "  cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE=${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake"
echo ""
echo "Or use CMake presets which are already configured with vcpkg:"
echo "  cmake --preset=dev"
