#!/bin/bash -ex
sudo apt-key adv --fetch-keys https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public
echo "deb https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/ focal main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get -y install --no-install-recommends adoptopenjdk-11-openj9
java --version
