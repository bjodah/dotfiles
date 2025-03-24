#!/bin/bash

if ( [[ "$DISPLAY" == localhost* ]] || [[ "$DISPLAY" == "" ]] ) && [[ $- == *i* ]]; then
    socket_path="$XDG_RUNTIME_DIR/my_ssh_socket"
    if [ -e "${socket_path}" ]; then
        source "${socket_path}.sh" >/dev/null
    elif [ -z "$SSH_AUTH_SOCK" ] ; then
	ssh-agent -s -a "$socket_path" >"${socket_path}.sh"
        source "${socket_path}.sh"
        ssh-add ~/.ssh/github_id_rsa
    fi
fi
