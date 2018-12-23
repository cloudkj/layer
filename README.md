layer - neural network inference from the command line
------------------------------------------------------

**`layer`** is a program for doing neural network inference the Unix way. Many
modern neural network operations are sequential, unidirectional streams of data
processed by pipelines of filters. The computations at each layer in these
neural networks are equivalent to an invocation of the program, and multiple
invocations can be chained together to represent the entirety of these neural
networks.

`layer` applies the Unix philosophy to neural network inference. Each type of
a neural network layer is a distinct `layer` subcommand. Simple text streams of
delimited numeric values serve as the interface between different layers of a
neural network. Each invocation of `layer` does one thing: it feeds the numeric
input values forward through an instantiation of a neural network layer, and
emits the resulting numeric values.

TODO expand upon Unix principles
