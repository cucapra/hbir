BUILD = dune build && dune install
RUN_F1 = lotus -f1
COMPILE = $(BUILD) && $(RUN_F1)


default: gemm

vvadd: 
	$(COMPILE) examples/f1-benchmarks/vvadd.hbir

gemm:
	$(COMPILE) examples/f1-benchmarks/gemm.hbir

collab-filter:
	$(COMPILE) examples/f1-benchmarks/collab-filter.hbir

clean:
	rm -rf f1-gen sim-gen gcc-gen
