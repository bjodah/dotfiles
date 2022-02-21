#!/bin/bash
set -euxo pipefail
if [[ ! -d ${THIS_RUNDIR:-""} ]]; then
    THIS_RUNDIR="$(mktemp -d)"
    # trap "rm -r \"${THIS_RUNDIR}\"" EXIT INT TERM
fi
CONTAINER_FOLDER=gccemacs-doom
# THIS_FOLDER=$(dirname $(realpath $BASH_SOURCE))
# THIS_FNAME=$(basename $(realpath $BASH_SOURCE))
if [[ ! -e CMakeLists.txt ]]; then
    >&2 echo "Not in a CMake-based source folder?"
fi
THIS_BUILD="${THIS_BUILD:-build-$CONTAINER_FOLDER}"
if [[ -e compile_commands.json ]]; then
    if [[ -L compile_commands.json && $(readlink compile_commands.json) == $THIS_BUILD/compile_commands.json ]]; then
       :
    else
        >&2 echo "Please move compile_commands.json"
        exit 1
    fi
fi
if [[ ! -d "$THIS_BUILD" ]]; then
    mkdir $THIS_BUILD
fi
cat <<EOF>$THIS_RUNDIR/.tmux.conf
unbind C-b
set -g prefix 'C-\'
bind 'C-\' send-prefix
set -g mouse on
set -g default-terminal "screen-256color"
set -g status-style "bg=blue"
set -g remain-on-exit on  # cf. respawn-pane & respawn-pane -k
set -g history-limit 15000  # default is 2000 lines
EOF

cat <<EOF>$THIS_RUNDIR/launch-emacs.sh
#!/bin/bash
set -euxo pipefail
mkdir -p ~/.cache/clangd
ln -fs ~/.cache/clangd $(pwd)/$THIS_BUILD/clangd
while ! test -e $(pwd)/compile_commands.json; do sleep 1; done
/install_dir/bin/emacs --eval '(load \"/opt/my-rundir/launch-emacs.el\")'
EOF
chmod +x $THIS_RUNDIR/launch-emacs.sh
cat <<EOF>$THIS_RUNDIR/launch-tmux.sh
#!/bin/bash
set -euxo pipefail
tmux -f /opt/my-rundir/.tmux.conf -2 -S tmux.sock \
     new -s ${CONTAINER_FOLDER}-$(basename $(dirname $(realpath $BASH_SOURCE))) \
     "cmake \\
             -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \\
             -G Ninja \\
             -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \\
             $CMAKE_ARGS \\
             -B $THIS_BUILD \\
             -S $(pwd) ; \\
      ln -fs $THIS_BUILD/compile_commands.json . && cmake --build $THIS_BUILD" \; \
     new-window "/opt/my-rundir/launch-emacs.sh" \; \
     new-window "ttyd --port 7682 tmux -f /opt/my-rundir/.tmux.conf -2 -S tmux.sock"
EOF
chmod +x $THIS_RUNDIR/launch-tmux.sh
cat <<EOF>$THIS_RUNDIR/launch-emacs.el
(progn
  ;;(load-file "/root/.emacs.d/init.el")
  (require 'lsp-mode)
  (lsp-workspace-folders-add "/usr/include/c++/10/")
  (lsp-workspace-folders-add "$(pwd)")
)
EOF

if [[ ! -d ~/.ccache/ ]]; then
    mkdir ~/.ccache/
fi
{  # this scope saves us from surprises if editing this file during podman executiong below
    bjodah \
        --container-folder $CONTAINER_FOLDER \
        --name host-dev-env-gccemacs-doom-cmake \
        --volume ~/.ccache:/root/.ccache \
        --volume "$THIS_RUNDIR":/opt/my-rundir/ \
        --publish 7682:7682 \
        -e THIS_IS_RUNNING_IN_CONTAINER=1 \
        -e CXX=clang++-14 \
        -- /opt/my-rundir/launch-tmux.sh
    exit 0
        # --volume "$THIS_FOLDER":/opt/my-scripts/ \

}

