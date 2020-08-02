DIR=$(ls -d $HOME/vc/centstructor-dockerfile/ 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
