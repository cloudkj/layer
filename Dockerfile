FROM ubuntu

RUN apt-get update && \
    apt-get install -y chicken-bin gcc libblas-dev bc ruby-full && \
    chicken-install blas getopt-long input-parse

CMD ["csi", "-version"]
