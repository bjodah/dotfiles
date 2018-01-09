GEMBIN=$(ls "$HOME"/.gem/ruby/*/bin | tail -n 1)
if [ -d "$GEMBIN" ]; then
    export PATH="$GEMBIN:$PATH"
fi
