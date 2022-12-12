sudo apt-get --assume-yes --no-install-recommends install apt-file nvidia-cuda-toolkit gimp git rsync curl gnupg2 iotop htop podman aufs-tools libsfml-dev python3-pyglet python3-pygame-sdl2 kdenlive gocryptfs kitty trickle qemu-system-ppc64 qemu-utils cloud-image-utils likwid xcape
sudo apt-file update

curl -sS https://download.spotify.com/debian/pubkey_5E3C45D7B312C643.gpg | sudo apt-key add - 
echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt-get update && sudo apt-get install spotify-client


# sudo dpkg -i google-chrome-stable_current_amd64.deb
# sudo dpkg -i GlobalProtect_deb-5.0.9.0-3.deb

# sudo apt install libgl1-mesa-glx libegl1-mesa libegl1-mesa libxcb-xtest0
# sudo dpkg -i zoom_amd64.deb

# sudo dpkg -i RCMeetingsClientSetup.deb

# https://www.microsoft.com/en-us/microsoft-365/microsoft-teams/download-app#desktopAppDownloadregion
sudo dpkg -i teams_*_amd64.deb

wget -O- https://updates.signal.org/desktop/apt/keys.asc |  sudo apt-key add -
echo "deb [arch=amd64] https://updates.signal.org/desktop/apt xenial main" |  sudo tee -a /etc/apt/sources.list.d/signal-xenial.list
sudo apt update && sudo apt install signal-desktop

cargo install gping
python3 -m pip install twine && ln -s ~/doc/it/login/.pypirc ~/.pypirc


curl -Ls https://github.com/ankitrohatgi/WebPlotDigitizer/archive/refs/tags/v4.5.tar.gz | tar xz -C /opt/


sudo add-apt-repository ppa:hluk/copyq
sudo apt update
sudo apt install copyq


# sudo add-apt-repository ppa:mozillateam/ppa
# sudo apt-get update
# sudo apt install firefox-esr


# Github CLI
# https://github.com/cli/cli/blob/trunk/docs/install_linux.md
type -p curl >/dev/null || sudo apt install curl -y
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
&& sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
&& echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
&& sudo apt update \
&& sudo apt install gh -y
