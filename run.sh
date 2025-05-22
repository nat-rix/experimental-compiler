#!/usr/bin/env sh
target/release/lx-compiler -fconstant-propagation -o "$2" -- "$1"
