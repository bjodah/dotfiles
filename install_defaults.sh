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
    DESTFILE=$HOME_DIR/${f#./per-file/}
    echo $DESTFILE
    if [ -L "$DESTFILE" ]; then
	rm "$DESTFILE"
    elif [ -e "$DESTFILE" ]; then
        >&2 echo "File already exists, skipping: $DESTFILE"
        continue
    fi
    DESTDIR=$(dirname $DESTFILE)
    if [[ ! -d "$DESTDIR" ]]; then
	if [[ -L "$DESTDIR" ]]; then
	    rm "$DESTDIR"  # remove broken link
	fi
        mkdir -p "$DESTDIR"
    fi
    ln -s "$ABS_REPO_PATH/${f#./}" "$DESTFILE"
    if [ -L "$DESTFILE" ]; then
        echo "Successfully symlinked file: $DESTFILE"
    else
        >&2 echo "ERROR: Exiting... Something went wrong creating symlink: $DESTFILE"
        exit 1
    fi
done

PER_LEAF_DIR="./per-leaf-dir"
# https://stackoverflow.com/a/62632786/790973
for d in $(find ./per-leaf-dir -type d | sed 's/$/\//' | sort -r | awk 'index(a,$0)!=1{a=$0;print}' | sed 's/\/$//' | sort);
do
    DESTLINK="$HOME_DIR/${d#./per-leaf-dir/}"
    if [[ -L "$DESTLINK" ]]; then
        rm "$DESTLINK"
    elif [[ -e "$DESTLINK" ]]; then
        >&2 echo "Directory already exists, skipping: $DESTLINK"
        continue
    fi
    DESTPARENT=$(dirname $DESTLINK)
    echo $DESTPARENT
    if [[ -L $DESTPARENT ]] && [[ ! -e $DESTPARENT ]]; then
        rm $DESTPARENT
    fi

    if [[ ! -d "$DESTPARENT" ]]; then
        mkdir -p "$DESTPARENT"
    fi
    ln -s "$ABS_REPO_PATH/${d#./}" "$DESTLINK"
    if [[ -L "$DESTLINK" ]]; then
        echo "Successfully symlinked dir: $DESTLINK"
    else
        >&2 echo "ERROR: Exiting... Something went wrong creating symlink: $DESTLINK"
        exit 1
    fi
done
