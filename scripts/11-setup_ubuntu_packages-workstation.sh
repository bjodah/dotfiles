sudo apt-get --assume-yes --no-install-recommends install nvidia-cuda-toolkit gimp htop iotop docker.io aufs-tools libsfml-dev podman


curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | sudo apt-key add -
echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt-get update && sudo apt-get install spotify-client

# sudo dpkg -i google-chrome-stable_current_amd64.deb
# sudo dpkg -i GlobalProtect_deb-5.0.9.0-3.deb

# sudo apt install libgl1-mesa-glx libegl1-mesa libegl1-mesa libxcb-xtest0
# sudo dpkg -i zoom_amd64.deb

# sudo dpkg -i RCMeetingsClientSetup.deb

wget -O- https://updates.signal.org/desktop/apt/keys.asc |  sudo apt-key add -
echo "deb [arch=amd64] https://updates.signal.org/desktop/apt xenial main" |  sudo tee -a /etc/apt/sources.list.d/signal-xenial.list
sudo apt update && sudo apt install signal-desktop
