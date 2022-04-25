#!/bin/bash
set -euo pipefail

show_help(){
    echo "--git-branch <name>    e.g.: 'emacs-28', 'master'"
    echo "--with-native-comp     Enable native compilation (libgccjit)"
    echo "--with-pgtk            Enable pure-gtk backend (req. gtk3)"
    echo "--build-root           a shallow clone of emacs' repo will be put under this dir"
    echo "--cflags               Specify CFLAGS, defaults to optimized build"
    echo "--make-command         default: make, e.g.: 'bear -- make'"
    echo ""
    echo "One of:"
    echo "--create-deb <outdir>  Build a debian dpkg/apt package, or:..."
    echo "--install <prefix>     Install into <prefix>"
    echo ""
    echo "Example:"
    echo "  \$ CC=gcc-10 ./$0 --git-branch emacs-28 -- --without-x && which emacs"
    echo "  /usr/local/bin/emacs"
}
EMACS_BRANCH="emacs-28"
WITH_NATIVE_COMP=0
WITH_PGTK=0
CREATE_DEB=0
INSTALL_PREFIX="/usr/local"
BUILD_ROOT=""
CFLAGS_GIVEN=""
MAKE_COMMAND="make"
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
	--make-command)
	    shift
	    MAKE_COMMAND=$1
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
	    CREATE_DEB_OUTDIR="$1"
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
if [[ $CREATE_DEB == 1 ]]; then
    if [[ $CREATE_DEB_OUTDIR == "" ]]; then
	>&2 echo "Got no --create-deb <dir>"
    fi
    if [[ ! -d "$CREATE_DEB_OUTDIR" ]]; then
	>&2 echo "No such output directory (--create-deb <dir>): $CREATE_DEB_OUTDIR"
	exit 1
    fi
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

CONFIGURE_FLAGS="\
 --with-dbus\
 --with-gif\
 --with-gpm=no\
 --with-jpeg\
 --with-json\
 --with-xml2\
 --with-modules\
 --with-png\
 --with-rsvg\
 --with-tiff\
 --with-xft\
 --with-xpm\
 $@"

# --with-imagemagick\


EMACS_FEATURES=""

if [[ $WITH_PGTK == 1 ]]; then
    CONFIGURE_FLAGS="$CONFIGURE_FLAGS --with-pgtk"
    EMACS_FEATURES="${EMACS_FEATURES}-pgtk"
else
    CONFIGURE_FLAGS="$CONFIGURE_FLAGS"  #  --with-xwidgets
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
( set -x; CC=${CC:-"gcc-12"} CXX=${CXX:-"g++-12"} CFLAGS="$CFLAGS_GIVEN" ./configure $CONFIGURE_FLAGS )

$MAKE_COMMAND -j $(nproc) $MAKE_FLAGS

if [[ $CREATE_DEB == 1 ]]; then
    EMACS_VERSION=$(sed -ne 's/AC_INIT(GNU Emacs, \([0-9.]\+\), .*/\1/p' configure.ac)
    EMACS_DEB_ROOT=$BUILD_ROOT/emacs${EMACS_FEATURES}_${EMACS_VERSION}
    mkdir -p $EMACS_DEB_ROOT$INSTALL_PREFIX
    make install prefix=$EMACS_DEB_ROOT$INSTALL_PREFIX
    mkdir $EMACS_DEB_ROOT/DEBIAN
    # libxml2-dev
    EMACS_DEB_DEP="libgif7, libotf1, libm17n-0, librsvg2-2, libtiff5, libjansson4, libacl1, libgmp10, libwebp7, libsqlite3-0, libxml2"
    EMACS_DEB_DESCR="Emacs $EMACS_VERSION ($EMACS_BRANCH)"
    if [[ $WITH_NATIVE_COMP == 1 ]]; then
	EMACS_DEB_DEP="$EMACS_DEB_DEP, libgccjit0"
	EMACS_DEB_DESCR="$EMACS_DEB_DESCR, with native compilation"
    fi
    if [[ $WITH_PGTK == 1 ]]; then
	EMACS_DEB_DEP="$EMACS_DEB_DEP, libgtk-3-0"
	EMACS_DEB_DESCR="$EMACS_DEB_DESCR, with pure-GTK"
    # else
    # 	EMACS_DEB_DEP="$EMACS_DEB_DEP, libwebkit2gtk-4.1-0"
    fi
    cat <<EOF >> $EMACS_DEB_ROOT/DEBIAN/control
Package: emacs${EMACS_FEATURES}
Version: ${EMACS_VERSION}
Section: base
Priority: optional
Architecture: $(dpkg-architecture --query DEB_TARGET_ARCH)
Depends: $EMACS_DEB_DEP
Maintainer: bjodah
Description: $EMACS_DEB_DESCR
    $CONFIGURE_FLAGS 
EOF
    dpkg-deb --build $EMACS_DEB_ROOT
    mv ${BUILD_ROOT}/emacs${EMACS_FEATURES}_${EMACS_VERSION}.deb "${CREATE_DEB_OUTDIR}"
elif [[ $INSTALL_PREFIX != "" ]]; then
    sudo make install
    make clean
else
    >&2 echo "Unknown state, exiting."
    exit 1
fi
