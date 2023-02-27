#!/bin/bash
set -euo pipefail
IFS=$' \t\n'

if [[ $1 == "Debug" ]]; then
    gdb -q -return-child-result --batch \
        -ex 'handle SIGHUP nostop pass' \
        -ex 'handle SIGQUIT nostop pass' \
        -ex 'handle SIGPIPE nostop pass' \
        -ex 'handle SIGALRM nostop pass' \
        -ex 'handle SIGTERM nostop pass' \
        -ex 'handle SIGUSR1 nostop pass' \
        -ex 'handle SIGUSR2 nostop pass' \
        -ex 'handle SIGCHLD nostop pass' \
        -ex 'set style enabled on' \
        -ex 'set print frame-arguments all' \
        -ex 'run' \
        -ex 'thread apply all bt -frame-info source-and-location -full' \
        --args cmake-build-$1/tests
else
  cmake-build-$1/tests
fi

