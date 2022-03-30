#!/bin/bash
cd /opt
git clone --depth 1 https://github.com/compiler-explorer/compiler-explorer
cd compiler-explorer
make
