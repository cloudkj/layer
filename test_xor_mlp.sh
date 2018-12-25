#!/bin/bash

expected="0.00005472969860420562
0.99951660633087158203
0.99951684474945068359
0.00060950592160224915"

printf "0,0\n0,1\n1,0\n1,1" \
    | ./layer dense -w test/weights.xor_mlp.0.csv -b test/biases.xor_mlp.0.csv --input-shape=2 -f tanh \
    | ./layer dense -w test/weights.xor_mlp.1.csv -b test/biases.xor_mlp.1.csv --input-shape=3 -f sigmoid \
    | xargs printf "%.12f\n" \
    | paste -d- - <(printf "$expected") \
    | bc \
    | ruby -e 'STDIN.each_line { |line| raise "Expected and actual differ by > 1E-6: " + line unless line.to_f.abs < 1E-6 }'
