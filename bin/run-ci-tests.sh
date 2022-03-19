#!/bin/bash

/usr/local/bin/install-quicklisp

# configure ASDF to find our files in $WORKSPACE
mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"${WORKSPACE}\")" > ~/.config/common-lisp/source-registry.conf.d/workspace.conf

exec "${WORKSPACE}/entrypoint.sh" "$@"
