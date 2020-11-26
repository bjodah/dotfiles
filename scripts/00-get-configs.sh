#!/bin/bash
set -euxo pipefail

mkdir -p doc/
scp -rp -P ${PORT} bjorn@${MOSSE}:~/doc/3d3s ~/doc/
cd ~/.ssh
cp -s ~/doc/3d3s/$(hostname)/ssh/* .
cp -s ~/doc/3d3s/share/ssh/* .

mkdir ~/.unison
cd ~/.unison/
cp -s ~/doc/3d3s/$(hostname)/unison/* .

unison ${WORK}
unison ${MOSSE}

python3 -m pip install ghcloneall
mkdir ~/vc/
cd ~/vc/
ghcloneall --user bjodah --include-private
