# -*- mode: shell-script -*-

rmb() {  # "remove build"
    rm -r ./build/
}
cbd() {  # "create build"
    mkdir -p ./build
    cd ./build
}
rcbd() { # "recreate build"
    BASEPWD=$(basename $PWD)
    if [[ $BASEPWD == build* ]]; then
        cd ../; rm -r "./$BASEPWD/"; mkdir "./$BASEPWD"; cd "./$BASEPWD"
    else
        >&2 echo "Not in a directory named 'build*'"
    fi
}
rmtmp() {
    rm -r ./tmp/
}
rmcm() {
    rm -r ./CMakeFiles/ ./CMakeCache.txt
}
vv() {
    # list folder sizes of argument 1 (default *)
    files=${@:-"*"} # vv !(.gvfs||afs) misses e.g. "~/VirtualBox VMs"
    du -ks $files | sort -n | cut -f2 | xargs -d '\n' du -sh
}
pygrep() {
    grep --include "*.py" "${1}" -R .
}

fnd() {
    find . -iname "*$@"
}
grp() {
    set -x
    grep --include "*$1" ${@:2} -R .
}
mkcd() {
    ( set -x; mkdir $@; cd ${@: -1} )
}

et29() {
    emx --client --use 29 -t $@  
}
ec29() {
    emx --client --use 29 $@  -- --x11
}
