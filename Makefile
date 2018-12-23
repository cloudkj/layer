VERSION?=0.1.0

main: *.scm
	csc -c core.scm options.scm util.scm conv.scm dense.scm pool.scm
	csc main.scm core.o options.o util.o conv.o dense.o pool.o -o layer

deploy: *.scm
	csc -deploy *.scm -o build
	chicken-install -deploy -p build blas getopt-long input-parse
	mv build/build build/layer
	mv build layer-$(VERSION)
	tar czf layer-$(VERSION).tar.gz layer-$(VERSION)

clean:
	rm -rf layer *.o
