#!/bin/bash
if ( [[ "$DISPLAY" == localhost* ]] || [[ "$DISPLAY" == "" ]] ) && [[ $- == *i* ]]; then
    if [ -z "$SSH_AUTH_SOCK" ] ; then
	eval `ssh-agent -s`
        ssh-add ~/.ssh/github_id_rsa
    fi
fi
