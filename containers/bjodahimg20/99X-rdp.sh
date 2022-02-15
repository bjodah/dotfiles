#!/bin/bash
cd ~/rovc
git clone git://github.com/FreeRDP/FreeRDP
cd FreeRDP
sudo apt install libsystemd-dev libavcodec-dev xsltproc libfuse3-dev libcups2-dev libpcsclite-dev libfaac-dev libswscale-dev libx264-dev libusb-1.0-0-dev
cmake -DCMAKE_INSTALL_PREFIX=/opt/freerdp3 -DBUILD_TYPE=Release -DWITH_SWSCALE=ON -DWITH_CAIRO=ON -DWITH_JPEG=ON -DWITH_X264=ON -DWITH_FAAC=ON -DWITH_FUSE3=ON -DWITH_CUPS=on -DWITH_PULSE=on -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -S . -B /build/freerdp 

curl -Ls https://gitlab.com/Remmina/Remmina/-/archive/v1.4.19/Remmina-v1.4.19.tar.bz2 | tar xj -C /build
cd /build/Remmina-v1.4.19/


sudo apt install --no-install-recommends  libx11-dev libxext-dev libxinerama-dev \
  libxcursor-dev libxdamage-dev libxv-dev libxkbfile-dev libasound2-dev libcups2-dev libxml2 libxml2-dev \
  libxrandr-dev libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev \
  libxi-dev libavutil-dev \
  libavcodec-dev libxtst-dev libgtk-3-dev libgcrypt20-dev libssh-dev libpulse-dev \
  libvte-2.91-dev libxkbfile-dev libtelepathy-glib-dev libjpeg-dev \
  libgnutls28-dev libavahi-ui-gtk3-dev libvncserver-dev \
  libappindicator3-dev intltool libsecret-1-dev libwebkit2gtk-4.0-dev libsystemd-dev \
  libsoup2.4-dev libjson-glib-dev libavresample-dev libsodium-dev \
  libusb-1.0-0-dev libpcre2-dev

CMAKE_PREFIX_PATH=/opt/freerdp3 cmake -DCMAKE_INSTALL_PREFIX=/opt/remmina -DBUILD_TYPE=Release -DWITH_FREERDP3=ON -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DWINPR_INCLUDE_DIR=/opt/freerdp3/include/winpr3/ -S . -B ./build/
cmake --build ./build/
cmake --install ./build/
