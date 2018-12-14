#!/bin/bash

# Test layer 1
echo "Layer 1"
expected1="1.0120909214019775,1.147718906402588,1.0172796249389648,0.9844224452972412,0.9614372849464417,0.9269811511039734,1.0560033321380615,0.9415908455848694,0.9388472437858582,0.9979854226112366,1.2195137739181519,1.1848801374435425,0.7960367798805237,0.8459836840629578,0.7911198139190674,1.0223075151443481,0.8284748792648315,0.7534469962120056,0.7680479288101196,0.6511459946632385"
expected1=$(echo $expected1 | tr ',' '\n')
cat test/cifar10_x.csv \
    | csi -script conv.scm -w test/weights.keras_cifar10_cnn.0 -b test/biases.keras_cifar10_cnn.0 --input-shape=32,32,3  --filter-shape=3,3 --num-filters=32 -a relu \
    | tr ',' '\n' \
    | (head -n 10; tail -n 10) \
    | diff -y --suppress-common-lines - <(echo "$expected1")

# Test layer 2
echo "Layer 2"
expected2="1.8877639770507812,0.8230810165405273,0.03670421615242958,0.08097217977046967,3.3788931369781494,1.2399506568908691,2.6708078384399414,0.869803249835968,1.257413625717163,2.465507984161377,3.0267560482025146,2.2217726707458496,0.0,1.4208928346633911,0.0,1.1560165882110596,1.2864243984222412,0.7643159031867981,1.4415825605392456,0.0"
expected2=$(echo $expected2 | tr ',' '\n')
cat test/cifar10_x.csv \
    | csi -script conv.scm -w test/weights.keras_cifar10_cnn.0 -b test/biases.keras_cifar10_cnn.0 --input-shape=32,32,3  --filter-shape=3,3 --num-filters=32 -a relu \
    | csi -script conv.scm -w test/weights.keras_cifar10_cnn.1 -b test/biases.keras_cifar10_cnn.1 --input-shape=30,30,32 --filter-shape=3,3 --num-filters=32 -a relu \
    | tr ',' '\n' \
    | (head -n 10; tail -n 10) \
    | diff -y --suppress-common-lines - <(echo "$expected2")

# Test layer 3
echo "Layer 3"
expected3="1.897286057472229,0.8494722247123718,0.08734359592199326,0.08097217977046967,3.4289982318878174,1.2610485553741455,2.6708078384399414,0.9394875764846802,1.3073047399520874,2.5357391834259033,3.059093475341797,2.2311010360717773,0.09556851536035538,1.4208928346633911,0.0,1.1560165882110596,1.2864243984222412,0.7643159031867981,1.494695782661438,0.0"
expected3=$(echo $expected3 | tr ',' '\n')
cat test/cifar10_x.csv \
    | csi -script conv.scm -w test/weights.keras_cifar10_cnn.0 -b test/biases.keras_cifar10_cnn.0 --input-shape=32,32,3  --filter-shape=3,3 --num-filters=32 -a relu \
    | csi -script conv.scm -w test/weights.keras_cifar10_cnn.1 -b test/biases.keras_cifar10_cnn.1 --input-shape=30,30,32 --filter-shape=3,3 --num-filters=32 -a relu \
    | csi -script pool.scm --input-shape=28,28,32 --filter-shape=2,2 --stride=2 -p max \
    | tr ',' '\n' \
    | (head -n 10; tail -n 10) \
    | diff -y --suppress-common-lines - <(echo "$expected3")
