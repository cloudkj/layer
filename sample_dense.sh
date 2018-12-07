#!/bin/sh
set -ex
cat sample/sample_x.csv \
    | csi -script dense.scm -w sample/sample_weights.0 -b sample/sample_biases.0 -a sigmoid \
    | csi -script dense.scm -w sample/sample_weights.1 -b sample/sample_biases.1 -a sigmoid
