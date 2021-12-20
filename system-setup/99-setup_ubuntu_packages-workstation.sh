sudo apt-get --assume-yes --no-install-recommends install apt-file nvidia-cuda-toolkit gimp iotop podman aufs-tools libsfml-dev podman python3-pyglet python3-pygame-sdl2 kdenlive gocryptfs kitty trickle qemu-system-ppc64 qemu-utils cloud-image-utils likwid
sudo apt-file update

curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | sudo apt-key add -
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
