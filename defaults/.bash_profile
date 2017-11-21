if [[ -d "$HOME/.bash_completion.d" ]]; then
    for f in "$HOME/.bash_completion.d/*"; do
        . "${f}"
    done
fi
