#!/bin/sh
set -ex

docker run -it -v "$PWD:/home/clinn" chicken-scheme bash
