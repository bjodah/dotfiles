#!/bin/bash -ex
apt-get --assume-yes --no-install-recommends install openjdk-17
[[ $(java --version | head -n 1) ~= "^openjdk 17*" ]]
