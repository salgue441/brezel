#!/usr/bin/env bash
set -euo pipefail

# Script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Make sure docker-compose is available
if ! command -v docker-compose &>/dev/null; then
  echo "Error: docker-compose is not installed. Please install it and try again."
  exit 1
fi

echo "Starting Brezel development container"

# Run the development container
cd "${PROJECT_ROOT}"
docker-compose run --rm dev
