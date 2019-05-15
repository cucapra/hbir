default: vvadd

BUILD = dune build && dune install
RUN_F1 = lotus -f1
COMPILE = $(BUILD) && $(RUN_F1)	

vvadd: 
	$(COMPILE) examples/f1-vvadd.hbir


bfs:
	$(COMPILE) examples/bfs.hbir


