sudo apt-get --assume-yes --no-install-recommends install software-properties-common
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt-get update
sudo apt-get --assume-yes upgrade
sudo apt-get --assume-yes --no-install-recommends install gcc-11 g++-11 gfortran-11 gdb
