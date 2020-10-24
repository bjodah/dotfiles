if [ -d "/opt/cargo" ]; then
    export CARGO_HOME=/opt/cargo
    export PATH="$CARGO_HOME:$PATH"
fi

if [ -d "/opt/rustup" ]; then
    export RUSTUP_HOME=/opt/rustup
    export PATH="$RUSTUP_HOME:$PATH"
fi
