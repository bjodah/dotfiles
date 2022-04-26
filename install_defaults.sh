#!/bin/bash
HOME_DIR=${HOME_DIR:-$HOME}
ABS_REPO_PATH=$(unset CDPATH && cd "$(dirname "$0")" && echo $PWD)
if ! grep bjodah/dotfiles $HOME_DIR/.profile >/dev/null; then
    cat "$ABS_REPO_PATH"/profile.append >>$HOME_DIR/.profile
    echo "Appended to ~/.profile"
fi
if ! grep bjodah/dotfiles $HOME_DIR/.bashrc >/dev/null; then
    cat "$ABS_REPO_PATH"/bashrc.append >>$HOME_DIR/.bashrc
    echo "Appended to ~/.bashrc"
fi

link_dest() {
    if [[ -L "$2" ]]; then
        rm "$2"
    elif [[ -e "$2" ]]; then
        >&2 echo "Directory already exists, skipping: $2"
        return
    fi
    DESTPARENT=$(dirname $2)
    if [[ -L $DESTPARENT ]] && [[ ! -e $DESTPARENT ]]; then
        rm $DESTPARENT
    fi

    if [[ ! -d "$DESTPARENT" ]]; then
        mkdir -p "$DESTPARENT"
    fi
    ln -s "$1" "$2"
    if [[ -L "$2" ]]; then
        echo "Successfully symlinked dir: $2"
    else
        >&2 echo "ERROR: Exiting... Something went wrong creating symlink: $2"
        exit 1
    fi
}

cd "$ABS_REPO_PATH"
PER_FILE="./per-file/"
for f in $(find $PER_FILE -type f); do
    if [[ $f == "./per-file/.gdbinit" ]]; then
        if [ -e "$HOME_DIR/.gdbinit" ]; then
            >&2 echo "~/.gdbinit already exists, skipping."
        else
            # echo "set auto-load safe-path /" >>~/.gdbinit
            echo "add-auto-load-safe-path $ABS_REPO_PATH/${f#./}" >~/.gdbinit
            echo "source \"$ABS_REPO_PATH/${f#./}\"" >>~/.gdbinit
        fi
        continue
    fi
    link_dest "$ABS_REPO_PATH/${f#./}" "$HOME_DIR/${f#./per-file/}"
done

PER_LEAF_DIR="./per-leaf-dir"
# https://stackoverflow.com/a/62632786/790973
for d in $(find ./per-leaf-dir -type d | sed 's/$/\//' | sort -r | awk 'index(a,$0)!=1{a=$0;print}' | sed 's/\/$//' | sort);
do
    link_dest "$ABS_REPO_PATH/${d#./}" "$HOME_DIR/${d#./per-leaf-dir/}"
done

for e in $(find ./depth-2 -maxdepth 2 -mindepth 2);
do
    link_dest "$ABS_REPO_PATH/${e#./}" "$HOME_DIR/${e#./depth-2/}"
done
