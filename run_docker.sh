#!/bin/sh
set -ex

docker run -it -v "$PWD:/home/layer" chicken-scheme bash
