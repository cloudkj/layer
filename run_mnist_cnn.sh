#!/bin/sh
set -ex

# TODO: add command for normalizing input
tail -n +2 mnist_test.csv \
    | ruby -e 'STDIN.each_line { |line| puts line.split(",").map { |x| x.to_f / 255.0 }.join(",") }' \
    | csi -script conv.scm --input-shape=28,28,1  --filter-shape=3,3 --num-filters=32 -a relu -w weights.keras_mnist_cnn.0 -b biases.keras_mnist_cnn.0 \
    | csi -script conv.scm --input-shape=26,26,32 --filter-shape=3,3 --num-filters=64 -a relu -w weights.keras_mnist_cnn.1 -b biases.keras_mnist_cnn.1 \
    | csi -script pool.scm --input-shape=24,24,64 --filter-shape=2,2 --stride=2 -p max \
    | ./dense              --input-shape=9216 -a relu    -w weights.keras_mnist_cnn.2 -b biases.keras_mnist_cnn.2 \
    | ./dense              --input-shape=128  -a softmax -w weights.keras_mnist_cnn.3 -b biases.keras_mnist_cnn.3
