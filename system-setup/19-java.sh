#!/bin/bash -ex
# sudo apt-key adv --fetch-keys https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public
# echo "deb https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/ focal main" | sudo tee -a /etc/apt/sources.list
# sudo apt-get update
# sudo apt-get -y install --no-install-recommends adoptopenjdk-11-openj9

#https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17%2B35/OpenJDK17-jdk_x64_linux_hotspot_17_35.tar.gz
#https://github.com/adoptium/installer/issues/330#issuecomment-926987965

java --version
