layer - neural network inference from the command line
------------------------------------------------------

**layer** is a program for doing neural network inference the Unix way. The
majority of modern neural network operations are sequential, unidirectional
streams of data processed by pipelines of filters. Each layer in a neural
network is equivalent to an invocation of **layer**, and invocations are
chained together to represent the neural network architecture.

layer applies the Unix philosophy to neural network inference. Simple streams
of text are used as the interface between different components of a neural
network, where the text is a list of numeric values typically delimited by a
special character such as the comma.

TODO expand upon Unix principles
