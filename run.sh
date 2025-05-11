#!/usr/bin/env sh
target/release/l1-compiler -fconstant-propagation -o "$2" -- "$1"
