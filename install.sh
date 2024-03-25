#!/usr/bin/env bash

git clone git@github.com:prog-lang/purist.git
cd purist
stack install
cd ..
rm -rf purist
