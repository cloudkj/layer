#!/bin/bash

function compare {
    local layer=$3
    local expected=$(echo "$1" | tr ',' '\n')
    local actual=$(echo "$2" \
                       | tr ',' '\n' \
                       | (head -n 10; tail -n 10))

    echo "$actual" \
        | paste -d- - <(echo "$expected") \
        | bc \
        | ruby -e 'STDIN.each_line { |line| raise "Expected and actual differ by > 1E-5: " + line unless line.to_f.abs < 1E-5 }'
    if [ $? -ne 0 ];
    then
        echo "Comparison failed at layer $layer"
    fi
}

layer0="../layer conv -w weights.keras_cifar10_cnn.0.csv -b biases.keras_cifar10_cnn.0.csv --input-shape=32,32,3  --filter-shape=3,3 --num-filters=32 -f relu"
layer1="../layer conv -w weights.keras_cifar10_cnn.1.csv -b biases.keras_cifar10_cnn.1.csv --input-shape=30,30,32 --filter-shape=3,3 --num-filters=32 -f relu"
layer2="../layer pool --input-shape=28,28,32 --filter-shape=2,2 --stride=2 -f max"

echo "Testing layer 0: $layer0"
expected0="1.0120909214019775,1.147718906402588,1.0172796249389648,0.9844224452972412,0.9614372849464417,0.9269811511039734,1.0560033321380615,0.9415908455848694,0.9388472437858582,0.9979854226112366,1.2195137739181519,1.1848801374435425,0.7960367798805237,0.8459836840629578,0.7911198139190674,1.0223075151443481,0.8284748792648315,0.7534469962120056,0.7680479288101196,0.6511459946632385"
actual0=$(cat cifar10_x.csv | $layer0)

compare "$expected0" "$actual0" 0

echo "Testing layer 1: $layer1"
expected1="1.8877639770507812,0.8230810165405273,0.03670421615242958,0.08097217977046967,3.3788931369781494,1.2399506568908691,2.6708078384399414,0.869803249835968,1.257413625717163,2.465507984161377,3.0267560482025146,2.2217726707458496,0.0,1.4208928346633911,0.0,1.1560165882110596,1.2864243984222412,0.7643159031867981,1.4415825605392456,0.0"
actual1=$(cat cifar10_x.csv | $layer0 | $layer1)

compare "$expected1" "$actual1" 1

echo "Testing layer 2: $layer2"
expected2="1.897286057472229,0.8494722247123718,0.08734359592199326,0.08097217977046967,3.4289982318878174,1.2610485553741455,2.6708078384399414,0.9394875764846802,1.3073047399520874,2.5357391834259033,3.059093475341797,2.2311010360717773,0.09556851536035538,1.4208928346633911,0.0,1.1560165882110596,1.2864243984222412,0.7643159031867981,1.494695782661438,0.0"
actual2=$(cat cifar10_x.csv | $layer0 | $layer1 | $layer2)

compare "$expected2" "$actual2" 2
