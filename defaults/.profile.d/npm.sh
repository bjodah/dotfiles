if which npm; then
    NPM_PREFIX=/opt/npm-global
    if [ ! -d ${NPM_PREFIX} ]; then
        mkdir ${NPM_PREFIX}
    fi
    if [[ "$(npm config get prefix)" != ${NPM_PREFIX} ]]; then
        npm config set prefix ${NPM_PREFIX}
    fi
    export PATH="${NPM_PREFIX}/bin:$PATH"
fi
