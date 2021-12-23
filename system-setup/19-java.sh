#!/bin/bash -ex

apt-get --assume-yes --no-install-recommends install openjdk-17
[[ $(java --version | head -n 1) ~= "^openjdk 17*" ]]


# sudo apt-key adv --fetch-keys https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public
# echo "deb https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/ focal main" | sudo tee -a /etc/apt/sources.list
# sudo apt-get update
# sudo apt-get -y install --no-install-recommends adoptopenjdk-11-openj9

#https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17%2B35/OpenJDK17-jdk_x64_linux_hotspot_17_35.tar.gz
#https://github.com/adoptium/installer/issues/330#issuecomment-926987965


# wget -qO - https://adoptium.jfrog.io/artifactory/api/security/keypair/default-gpg-key/public  | apt-key add -
# echo "deb https://adoptium.jfrog.io/artifactory/deb focal main" > /etc/apt/sources.list.d/adoptium.list
# apt update
# apt install temurin-17-jdk
