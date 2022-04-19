GEMBIN=$(ls -d "$HOME"/.gem/ruby/*/bin 2>/dev/null | tail -n 1)
if [ -d "$GEMBIN" ]; then
    export PATH="$GEMBIN:$PATH"
fi
