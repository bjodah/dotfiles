#!/bin/bash
set -euo pipefail

show_help(){
    echo "--git-branch <name>    e.g.: 'emacs-28', 'master'"
    echo "--with-native-comp     Enable native compilation (libgccjit)"
    echo "--with-pgtk            Enable pure-gtk backend (req. gtk3)"
    echo "--build-root           emacs will be cloned from this dir"
    echo "--cflags               Specify CFLAGS, defaults to optimized build"
    echo ""
    echo "One of:"
    echo "--create-deb            Build a debian dpkg/apt package, or:..."
    echo "--install <prefix>     Install into <prefix>"
}
EMACS_BRANCH="emacs-28"
WITH_NATIVE_COMP=0
WITH_PGTK=0
CREATE_DEB=0
INSTALL_PREFIX=""
BUILD_ROOT=""
CFLAGS_GIVEN=""
while [ $# -gt 0 ]; do
    case "$1" in
	-h|--help|\?)
	    show_help
	    exit 0
	    ;;
	--git-branch)
	    shift
	    EMACS_BRANCH=$1
	    shift
	    ;;
	--cflags)
	    shift
	    CFLAGS_GIVEN="$1"
	    shift
	    ;;
	--build-root)
	    shift
	    BUILD_ROOT=$1
	    shift
	    ;;
	--with-native-comp)
	    WITH_NATIVE_COMP=1
	    shift
	    ;;
	--with-pgtk)
	    WITH_PGTK=1
	    shift
	    ;;
	--create-deb)
	    CREATE_DEB=1
	    shift
	    ;;
	--install)
	    shift
	    INSTALL_PREFIX=$1
	    shift
	    ;;
	--)
	    shift
	    break
	    ;;
	*)
	    show_help
	    exit 1
	    ;;
    esac
done  
if [[ $CREATE_DEB == 0 && $INSTALL_PREFIX == "" ]]; then
    >&2 echo "Need either --create-deb or --install <prefix> as flag"
    exit 1
elif [[ $CREATE_DEB != 0 && $INSTALL_PREFIX != "" ]]; then
    >&2 echo "Cannot have both --create-deb and --install <prefix> specified"
    exit 1
fi
if [[ $BUILD_ROOT == "" ]]; then
    >&2 echo "Need to specify build root dir --build-root"
    exit 1
fi
mkdir -p ${BUILD_ROOT}
cd ${BUILD_ROOT}
if [[ $EMACS_BRANCH == "master" ]]; then
    EMACS_SRC_DIR="emacs-master"
else
    EMACS_SRC_DIR=$EMACS_BRANCH
fi
if [[ ! -e $EMACS_SRC_DIR ]]; then
    git clone --depth 1 --branch $EMACS_BRANCH https://github.com/emacs-mirror/emacs $EMACS_SRC_DIR
fi
cd $EMACS_SRC_DIR


# Resources:
# http://www.cesarolea.com/posts/emacs-native-compile/

./autogen.sh

if [[ $CFLAGS_GIVEN == "" ]]; then
    if [[ $(uname -m) == "x86_64" ]]; then
	export ARCH_FLAGS="-mtune=skylake -march=nehalem"
    else
	export ARCH_FLAGS=""
    fi
    CFLAGS_GIVEN="-O3 -pipe $ARCH_FLAGS -fomit-frame-pointer"
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
 --with-imagemagick\
 --with-modules\
 --with-json"
EMACS_FEATURES=""
if [[ $WITH_PGTK == 1 ]]; then
    CONFIGURE_FLAGS="$CONFIGURE_FLAGS --with-pgtk"
    EMACS_FEATURES="${EMACS_FEATURES}-pgtk"
else
    CONFIGURE_FLAGS="$CONFIGURE_FLAGS --with-xwidgets"
fi
if [[ $WITH_NATIVE_COMP == 1 ]]; then
    CONFIGURE_FLAGS="$CONFIGURE_FLAGS --with-native-compilation"
    EMACS_FEATURES="${EMACS_FEATURES}-gcc"
    MAKE_FLAGS="NATIVE_FULL_AOT=1"
else
    MAKE_FLAGS=""
fi
if [[ $INSTALL_PREFIX != "" ]]; then
    CONFIGURE_FLAGS="$CONFIGURE_FLAGS --prefix=$INSTALL_PREFIX"
fi
( set -x; \
  CC=gcc-12 CXX=g++-12 CFLAGS="$CFLAGS_GIVEN" ./configure \
    $CONFIGURE_FLAGS )

bear -- make -j $(nproc) $MAKE_FLAGS

if [[ $CREATE_DEB == 1 ]]; then
    EMACS_VERSION=$(sed -ne 's/AC_INIT(GNU Emacs, \([0-9.]\+\), .*/\1/p' configure.ac)
    EMACS_DEB_ROOT=$BUILD_ROOT/emacs${EMACS_FEATURES}_${EMACS_VERSION}
    mkdir -p $EMACS_DEB_ROOT/usr/local
    make install prefix=$EMACS_DEB_ROOT/usr/local
    mkdir $EMACS_DEB_ROOT/DEBIAN
    EMACS_DEB_DEP="libgif7, libotf1, libm17n-0, librsvg2-2, libtiff5, libjansson4, libacl1, libgmp10, libwebp6, libsqlite3-0"
    EMACS_DEB_DESCR="Emacs $EMACS_VERSION ($EMACS_BRANCH)"
    if [[ $WITH_NATIVE_COMP == 1 ]]; then
	EMACS_DEB_DEP="$EMACS_DEB_DEP, libgccjit0"
	EMACS_DEB_DESCR="$EMACS_DEB_DESCR, with native compilation"
    fi
    if [[ $WITH_PGTK == 1 ]]; then
	EMACS_DEB_DEP="$EMACS_DEB_DEP, libgtk-3-0"
	EMACS_DEB_DESCR="$EMACS_DEB_DESCR, with pure-GTK"
    fi
    echo "Package: emacs${EMACS_FEATURES}\n\
Version: ${EMACS_VERSION}\n\
Section: base\n\
Priority: optional\n\
Architecture: $(dpkg-architecture --query DEB_TARGET_ARCH)\n\
Depends: $EMACS_DEB_DEP\n\
Maintainer: bjodah\n\
Description: $EMACS_DEB_DESCR\n\
    $CONFIGURE_FLAGS" \
         >> $EMACS_DEB_ROOT/DEBIAN/control
    dpkg-deb --build $EMACS_DEB_ROOT
    mkdir -p /opt/deploy
    mv /opt/emacs-gcc-pgtk_*.deb /opt/deploy
elif [[ $INSTALL_PREFIX != "" ]]; then
    sudo make install
    make clean
else
    >&2 echo "Unknown state, exiting."
    exit 1
fi
