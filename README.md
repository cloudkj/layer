# layer - neural network inference from the command line

`layer` is a program for doing neural network inference the Unix way. Many
modern neural network operations can be represented as sequential,
unidirectional streams of data processed by pipelines of [filters](https://en.wikipedia.org/wiki/Filter_(software)).
The computations at each layer in these neural networks are equivalent to an
invocation of the `layer` program, and multiple invocations can be chained
together to represent the entirety of such networks.

`layer` applies the Unix philosophy to neural network inference. Each type of
a neural network layer is a distinct subcommand. Simple text streams of
delimited numeric values serve as the interface between different layers of a
neural network. Each invocation of `layer` does one thing: it feeds the numeric
input values forward through an instantiation of a neural network layer, then
emits the resulting numeric values.

## Installation

## Example

A multi-layer perceptron for XOR.

```shell
$ # Fully connected layer with three neurons
echo "-2.35546875,-2.38671875,3.63671875,3.521484375,-2.255859375,-2.732421875" > layer1.weights
echo "0.7958984375,0.291259765625,1.099609375" > layer1.biases

$ # Fully connected layer with one neuron
echo "-5.0625,-3.515625,-5.0625" > layer2.weights
echo "1.74609375" > layer2.biases

$ # Compute XOR for all possible binary inputs
echo -e "0,0\n0,1\n1,0\n1,1" \
    | ./layer dense -w layer1.weights -b layer1.biases --input-shape=2 -f tanh \
    | ./layer dense -w layer2.weights -b layer2.biases --input-shape=3 -f sigmoid
0.00129012749948779
0.99147053740106
0.991243357927591
0.0111237568184365
```

## Documentation

`layer` is currently implemented as a proof-of-concept and supports a limited
number of neural network layer types. The types of layers are currently limited
to feed-forward layers that can be modeled as sequential, unidirectional
pipelines.

Input values, weights and biases for parameterized layers, and output values
are all read and written in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order),
based on the shape parameters specified for each layer.

## License

Copyright © 2018-2019
