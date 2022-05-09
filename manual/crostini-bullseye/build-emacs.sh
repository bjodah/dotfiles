#!/bin/bash -e
#sudo apt-get install autoconf libgtk-3-dev libncurses-dev libgccjit-10-dev libjansson-dev libjansson4 libgccjit0 libxpm-dev libgif-dev libjpeg-dev libpng-dev libtiff-dev libx11-dev libncurses5-dev automake autoconf texinfo libgnutls28-dev
if [[ $1 == "--build-dev" ]]; then
    :
elif [[ $1 == "--install" ]]; then
    :
else
    >&2 echo "Unknown argument passed, pass eigher --build-dev or --install."
    exit 1
fi

mkdir -p /build
cd /build
if [[ ! -d emacs ]]; then
    git clone --depth 1 --branch master git://github.com/emacs-mirror/emacs  # --branch emacs-28
fi
cd emacs
# Resources:
# http://www.cesarolea.com/posts/emacs-native-compile/
#
if [[ ! -f configure ]]; then
    ./autogen.sh
fi

if [[ $(uname -m) == "x86_64" ]]; then
    export ARCH_FLAGS="-mtune=skylake -march=nehalem"
else
    export ARCH_FLAGS="-march=native -mtune=native"
fi

CONFIGURE_FLAGS="--with-dbus \
 --with-gif\
 --with-jpeg\
 --with-png\
 --with-rsvg\
 --with-tiff\
 --with-xft\
 --with-xpm\
 --with-gpm=no\
 --without-xwidgets\
 --with-modules\
 --with-json\
 --with-native-compilation\
 --with-pgtk"
 
CFLAGS='-O3 -pipe $ARCH_FLAGS -fomit-frame-pointer'
#--with-imagemagick
CC=gcc-10 CXX=g++-10 ./configure \
  $CONFIGURE_FLAGS \
  --prefix=/usr/local

make -j $(nproc) NATIVE_FULL_AOT=2

if [[ $1 == "--build-dev" ]]; then
    EMACS_VERSION=$(sed -ne 's/AC_INIT(GNU Emacs, \([0-9.]\+\), .*/\1/p' configure.ac)
    make install prefix=/opt/emacs-gcc-pgtk_${EMACS_VERSION}/usr/local
    mkdir emacs-gcc-pgtk_${EMACS_VERSION}/DEBIAN
    echo "Package: emacs-gcc-pgtk\n\
Version: ${EMACS_VERSION}\n\
Section: base\n\
Priority: optional\n\
Architecture: $(dpkg-architecture --query DEB_TARGET_ARCH)\n\
Depends: libgif7, libotf1, libgccjit0, libm17n-0, libgtk-3-0, librsvg2-2, libtiff5, libjansson4, libacl1, libgmp10, libwebp6, libsqlite3-0\n\
Maintainer: konstare\n\
Description: Emacs with native compilation and pure GTK\n\
    $CONFIGURE_FLAGS" \
         >> emacs-gcc-pgtk_${EMACS_VERSION}/DEBIAN/control
    dpkg-deb --build emacs-gcc-pgtk_${EMACS_VERSION}
    mkdir -p /opt/deploy
    mv /opt/emacs-gcc-pgtk_*.deb /opt/deploy
elif [[ $1 == "--install" ]]; then
    sudo make install
    make clean
else
    >&2 echo "Unknown argument passed, pass eigher --build-dev or --install."
    exit 1
fi
