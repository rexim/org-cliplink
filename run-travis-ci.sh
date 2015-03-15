#!/bin/bash

function finish {
    echo "Killing testing server..."
    kill $(jobs -p)
}

trap finish EXIT

echo "Starting testing server..."
./run-testing-server.py &
sleep 1

emacs --batch --no-site-file --no-splash -l ert --script integration-tests.el || exit 1
emacs --batch --no-site-file --no-splash -l ert --script unit-tests.el || exit 1
