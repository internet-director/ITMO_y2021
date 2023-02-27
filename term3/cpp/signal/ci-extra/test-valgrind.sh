#!/bin/bash
set -euo pipefail
IFS=$' \t\n'

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

valgrind --tool=memcheck --gen-suppressions=all --leak-check=full --show-leak-kinds=all --leak-resolution=med --track-origins=yes --vgdb=no --error-exitcode=1 --suppressions="${SCRIPT_DIR}/valgrind.suppressions" cmake-build-RelWithDebInfo/tests
