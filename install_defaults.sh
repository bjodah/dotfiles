#!/bin/bash
ABS_REPO_PATH=$(unset CDPATH && cd "$(dirname "$0")" && echo $PWD)
if ! grep bjodah/dotfiles $HOME/.profile >/dev/null; then
    cat "$ABS_REPO_PATH"/profile.append >>$HOME/.profile
    echo "Appended to ~/.profile"
fi
if ! grep bjodah/dotfiles $HOME/.bashrc >/dev/null; then
    cat "$ABS_REPO_PATH"/bashrc.append >>$HOME/.bashrc
    echo "Appended to ~/.bashrc"
fi

cd "$ABS_REPO_PATH"/defaults
for f in $(find . -maxdepth 1 -type f); do
    if [[ $f == ".gdbinit" ]]; then
        if [ -e "$HOME/.gdbinit" ]; then
            >&2 echo "~/.gdbinit already exists, skipping."
        else
            # echo "set auto-load safe-path /" >>~/.gdbinit
            echo "add-auto-load-safe-path $ABS_REPO_PATH/defaults/.gdbinit" >>~/.gdbinit
            echo "source \"$ABS_REPO_PATH/defaults/.gdbinit\""
        fi
        continue
    fi
    DESTFILE="$HOME/$(basename "$f")"
    if [ -e "$DESTFILE" ]; then
        if [ -L "$DESTFILE" ]; then
	    rm "$DESTFILE"
        else
            echo "File already exists, skipping: $DESTFILE"
            continue
        fi
    fi
    ln -s "$ABS_REPO_PATH/defaults/$f" "$DESTFILE"
    if [ -L "$DESTFILE" ]; then
        echo "Successfully symlinked $DESTFILE"
    else
        >&2 echo "ERROR: Exiting... Something went wrong creating symlink: $DESTFILE"
        exit 1
    fi
done

for d in $(find . -maxdepth 1 ! -path . -type d);
do
    for f in $(find $d -maxdepth 1 ! -path $d ! -path \*.gitattributes);
    do
        DESTDIR="$HOME/$(dirname "$f")"
        DESTFILE="$DESTDIR/$(basename $f)"
        if [ -e "$DESTFILE" ]; then
            if [ -L "$DESTFILE" ]; then
	        rm "$DESTFILE"
            else
                echo "File already exists, skipping: $DESTFILE"
                continue
            fi
        fi
        if [ ! -e $DESTDIR ]; then
            "Creating directory: $DESTDIR"
            mkdir -p $DESTDIR
        fi
        ln -s "$ABS_REPO_PATH"/defaults/$f $DESTFILE
        if [ -L "$DESTFILE" ]; then
            echo "Successfully symlinked $DESTFILE"
        else
            >&2 echo "ERROR: Exiting... Something went wrong creating symlink: $DESTFILE"
            exit 1
        fi
    done
done
