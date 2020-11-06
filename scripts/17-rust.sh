#!/bin/bash -eux
ls -dl $RUSTUP_HOME
ls -dl $CARGO_HOME
cd /tmp
curl -LOs https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init
chmod +x rustup-init
./rustup-init -y --no-modify-path --default-toolchain stable
rm rustup-init
rustup --version
cargo --version
rustc --version
rustup component add rustfmt
