#!/bin/bash
set -euxo pipefail
cd /tmp
curl -LOs https://github.com/BurntSushi/ripgrep/releases/download/12.1.1/ripgrep_12.1.1_amd64.deb
dpkg -i ripgrep_12.1.1_amd64.deb
rm ripgrep_12.1.1_amd64.deb

curl -LOs https://github.com/sharkdp/fd/releases/download/v8.2.1/fd_8.2.1_amd64.deb
dpkg -i fd_8.2.1_amd64.deb
rm fd_8.2.1_amd64.deb
