#!/bin/bash

function finish {
    echo "Killing testing server..."
    kill $(jobs -p)
}

trap finish EXIT

echo "Starting testing server..."
cd test-data/site
python -m SimpleHTTPServer 3001 &
cd ../..
sleep 1

emacs --batch --no-site-file --no-splash -l ert --script integration-tests.el || exit 1
emacs --batch --no-site-file --no-splash -l ert --script unit-tests.el || exit 1
