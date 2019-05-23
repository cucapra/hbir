LOTUS := dune exec lotus --

default: collab-filter

vvadd:
	$(LOTUS) -f1 -wrapper examples/f1-benchmarks/vvadd.hbir

gemm:
	$(LOTUS) -f1 -wrapper examples/f1-benchmarks/gemm.hbir

collab-filter:
	$(LOTUS) -f1 -wrapper examples/f1-benchmarks/collab-filter.hbir

clean:
	rm -rf f1-gen sim-gen gcc-gen
