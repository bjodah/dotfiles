#!/bin/bash
set -eux
mkdir -p $RUSTUP_HOME
mkdir -p $CARGO_HOME
if [ ! -e "$CARGO_HOME/bin/rustc" ]; then
    cd /tmp
    curl -LOs https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init
    chmod +x rustup-init
    ./rustup-init -y --no-modify-path --default-toolchain stable
    rm rustup-init
fi
export PATH="$CARGO_HOME/bin:$PATH"
rustup --version
cargo --version
rustc --version
source $CARGO_HOME/env
rustup install stable
rustup default stable
rustup component add rustfmt
