#!/bin/sh
set -eux

pushd src/low-level/low-level-rust

# Vendor all the rust dependencies
cargo vendor

# Rename the config.toml file so that cargo will use it and use vendored
# dependencies rather than trying to download them. This is needed because
# programs can't access the internet from within an opam sandbox.
mv .cargo/vendor_dependencies_config.toml .cargo/config.toml

popd

# Update invocation of cargo to prevent it from trying to use the internet
perl -i -pe 's/cargo build/cargo build --offline/g' src/low-level/dune
