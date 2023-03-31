#!/bin/bash

if ! $(sudo virsh list) | grep ubuntu22.04; then
    sudo virsh start ubuntu22.04
fi

# XAUTHORITY=$HOME/.Xauthority MOZ_NO_REMOTE=1 firefox -no-remote -no-xshm

ssh -t argus-ubuntu 'tmux a || tmux new -s work globalprotect \; split-window -v \; new-window "XAUTHORITY=~/.Xauthority firefox -no-remote"'

