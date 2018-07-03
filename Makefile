# FIXME strawman targets
MLTON := mlton
# MLTON_FLAGS := -verbose 3
MLTON_FLAGS := -const 'Exn.keepHistory true'

POLY := $(HOME)/src/repos/polyml/poly

.PHONY: poly ROOT

ROOT:
	$(MLTON) $(MLTON_FLAGS) ROOT.mlb

poly:
	$(POLY) --use ROOT-polyml.sml

clean:
	rm -f ROOT
