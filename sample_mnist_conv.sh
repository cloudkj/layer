#!/bin/sh'set -ex
cat mnist_x.csv \
    | csi -script conv.scm -w weights.keras_mnist_cnn.0 --input-shape=28,28 --filter-shape=8,8 --num-filters=2 \
    | csi -script conv.scm -w weights.keras_mnist_cnn.1 --input-shape=21,21 --filter-shape=3,3 --num-filters=5
