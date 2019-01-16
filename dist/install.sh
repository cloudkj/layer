#!/usr/bin/env bash
BASE_PATH=/usr/local
INSTALL_PATH=$BASE_PATH/layer

mkdir -p "$INSTALL_PATH/bin"
cp -r * "$INSTALL_PATH/bin"
ln -s "$INSTALL_PATH/bin/layer" "$BASE_PATH/bin/layer"
