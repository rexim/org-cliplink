#!/bin/sh

cd test-data/site
python -m SimpleHTTPServer 3001 &
sleep 1
cd ../..

emacs --batch --script integration-tests.el || exit 1
emacs --batch --script unit-tests.el || exit 1
