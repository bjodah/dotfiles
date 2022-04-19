# https://github.com/mstorsjo/msvc-wine
apt-get install -y wine64-development python msitools python-simplejson python-six ca-certificates winbind

git clone https://github.com/mstorsjo/msvc-wine /build/msvc-wine
cd /build/msvc-wine

./vsdownload.py --accept-license --dest /msvc-wine/msvc
./install.sh /msvc-wine/msvc

wine64 wineboot --init && \
    while pgrep wineserver > /dev/null; do sleep 1; done

cd /build/
git clone https://gitlab.kitware.com/mstorsjo/cmake.git
cd cmake
git checkout 844ccd2280d11ada286d0e2547c0fa5ff22bd4db
mkdir build 
cd build
../configure --prefix=/msvc-wine/cmake --parallel=$(nproc) -- -DCMAKE_USE_OPENSSL=OFF
make -j$(nproc)
make install

