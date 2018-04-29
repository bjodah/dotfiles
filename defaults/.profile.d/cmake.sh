if [ -d /opt/cmake-*-*/ ]; then
    cmake_bin=$(ls -d /opt/cmake-*-*/ | head -n 1)bin
    if [ -d "$cmake_bin" ]; then
        export PATH="$cmake_bin:$PATH"
    fi
fi
