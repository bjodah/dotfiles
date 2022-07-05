#!/bin/bash
show_help() {
    echo "Host a development environment (emacs) in a container (podman) for a CMake-based project."
    echo ""
    echo "Usage:"
    echo "--host-ttyd <port>   host a webserver (xterm.js) of tmux session on <port>"
    echo "--no-evil            Disable Evil mode (i.e. use emacs keybindings instead of vim's)"
    echo ""
    echo "Example:"
    echo ""
    echo "   $ CMAKE_ARGS=\"-DCMAKE_BUILD_TYPE=Debug\" host-devenv-gccemacs-doom-cmake.sh -- --x11"
}
HOST_TTYD=""
NO_EVIL=0
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help|\?)
            show_help
            exit 0
            ;;
        --host-ttyd)
            shift
            HOST_TTYD=$1
            shift
            ;;
        --no-evil)
            NO_EVIL=1
            shift
            ;;
        --)
            shift;
            break;
            ;;
        *)
            >&2 echo "Unrecognized flag: $1, pass --help fpr more info, aborting..."
            exit 1
    esac
done
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

if [[ $HOST_TTYD != "" ]]; then
    echo "\; \\"<$(head -c -1 $THIS_RUNDIR/launch-tmux.sh) >$THIS_RUNDIR/launch-tmux.sh
    echo "new-window \"ttyd --port $HOST_TTYD tmux -f /opt/my-rundir/.tmux.conf -2 -S tmux.sock\"" >>$THIS_RUNDIR/launch-tmux.sh
    EMACS_FLAGS="-nw"
else
    EMACS_FLAGS=""
fi


cat <<EOF>$THIS_RUNDIR/launch-emacs.sh
#!/bin/bash
set -euxo pipefail
if [[ $NO_EVIL == 1 ]]; then
   # https://github.com/hlissner/doom-emacs/blob/develop/modules/editor/evil/README.org#removing-evil-mode
   sed -i '/\(evil[a-z+]*\)/d' ~/.doom.d/init.el
   ~/.emacs.d/bin/doom sync
fi
while ! test -e $(pwd)/compile_commands.json; do sleep 1; done
/install_dir/bin/emacs $EMACS_FLAGS --eval '(load "/opt/my-rundir/launch-emacs.el")'
EOF
chmod +x $THIS_RUNDIR/launch-emacs.sh
cat <<EOF>$THIS_RUNDIR/launch-tmux.sh
#!/bin/bash
set -euxo pipefail
tmux -f /opt/my-rundir/.tmux.conf -2 -S tmux.sock \
     new -s ${CONTAINER_FOLDER}-$(basename $(dirname $(realpath $BASH_SOURCE))) \
     "set -x \\
     ; cmake \\
             -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \\
             -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \\
             $CMAKE_ARGS \\
             -B $THIS_BUILD \\
             -S $(pwd) \\
      && ln -fs $THIS_BUILD/compile_commands.json . \\
      && cmake --build $THIS_BUILD" \; \
     new-window "/opt/my-rundir/launch-emacs.sh"
EOF

if [[ $HOST_TTYD != "" ]]; then
    echo "\; \\"<$(head -c -1 $THIS_RUNDIR/launch-tmux.sh) >$THIS_RUNDIR/launch-tmux.sh
    echo "new-window \"ttyd --port $HOST_TTYD tmux -f /opt/my-rundir/.tmux.conf -2 -S tmux.sock\"" >>$THIS_RUNDIR/launch-tmux.sh
fi
chmod +x $THIS_RUNDIR/launch-tmux.sh


cat <<EOF>$THIS_RUNDIR/launch-emacs.el
(progn
  (require 'lsp-mode)
  (lsp-workspace-folders-add "/usr/include/c++/10/")
  (lsp-workspace-folders-add "$(pwd)")
  ${EMACS_COMMANDS:-""}
)
EOF

if [[ ! -d ~/.ccache/ ]]; then
    mkdir ~/.ccache/
fi
THIS_CLANGD_CACHE=${THIS_CLANGD_CACHE:-~/.cache/clangd}
if [[ ! -e $THIS_CLANGD_CACHE ]]; then
    mkdir $THIS_CLANGD_CACHE
fi

THIS_CCACHE=${THIS_CLANGD_CACHE:-~/.ccache}
if [[ ! -e $THIS_CCACHE ]]; then
    mkdir $THIS_CCACHE
fi


{  # this scope saves us from surprises if editing this file during podman executiong below
    podrun \
        $* \
        --container-folder $CONTAINER_FOLDER \
        --name host-dev-env-gccemacs-doom-cmake \
        --volume $THIS_CCACHE:/root/.ccache \
        --volume $THIS_CLANGD_CACHE:/root/.cache/clangd \
        --volume "$THIS_RUNDIR":/opt/my-rundir/ \
        --publish 7682:7682 \
        -e THIS_IS_RUNNING_IN_CONTAINER=1 \
        -e CXX=clang++-14 \
        -- /opt/my-rundir/launch-tmux.sh
    exit 0
        # --volume "$THIS_FOLDER":/opt/my-scripts/ \

}

