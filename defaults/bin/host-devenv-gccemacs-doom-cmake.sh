#!/bin/bash
set -euxo pipefail
THIS_FOLDER=$(dirname $(realpath $BASH_SOURCE))
THIS_FNAME=$(basename $(realpath $BASH_SOURCE))
THIS_BUILD="${THIS_BUILD:-build-gccemacs-doom}"
if [[ ! -e CMakeLists.txt ]]; then
    >&2 echo "Not in a CMake-based source folder?"
fi

if [[ ${THIS_IS_RUNNING_IN_CONTAINER:-0} == 1 ]]; then

    cat <<EOF>>~/.tmux.conf
unbind C-b
set -g prefix 'C-\'
bind 'C-\' send-prefix
set -g mouse on
set -g default-terminal "screen-256color"
set -g status-style "bg=blue"
set -g remain-on-exit on  # cf. respawn-pane & respawn-pane -k
set -g history-limit 15000  # default is 2000 lines
EOF

    tmux -2 -S tmux.sock \
         new -s $THIS_FOLDER-$(dirname $(realpath $THIS_BUILD)) \
         "cmake \\
                 -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \\
                 -G Ninja \\
                 -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \\
                 $CMAKE_ARGS \\
                 -B $THIS_BUILD \\
                 -S $(pwd) ; \\
          ln -s $THIS_BUILD/compile_commands.json ." \; \
         new-window "emacs --eval '(load \"$(pwd)/launch-emacs.el\")'" \; \
         new-window "ttyd tmux -2 -S tmux.sock"
else
    if [[ ! -e launch-emacs.el ]]; then
        cat <<EOF>launch-emacs.el
(progn
  ;;(load-file "/root/.emacs.d/init.el")
  (require 'lsp-mode)
  (lsp-workspace-folders-add "/usr/include/c++/10/")
  (lsp-workspace-folders-add "$(pwd)")
)
EOF
    fi
    if [[ ! -d ~/.ccache/ ]]; then
        mkdir ~/.ccache/
    fi
    {  # this scope saves us from surprises if editing this file during podman executiong below
        bjodah \
            --container-folder gccemacs-doom \
            --name host-dev-env-gccemacs-doom-cmake \
            --volume $THIS_FOLDER:/opt/my-scripts/ \
            --volume ~/.ccache:/root/.ccache \
            --publish 7681:7681 \
            -e THIS_IS_RUNNING_IN_CONTAINER=1 \
            -e CMAKE_ARGS \
            -e CXX=clang++-14 \
            -- /opt/my-scripts/$THIS_FNAME
        exit 0
    }
fi
