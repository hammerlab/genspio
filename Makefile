
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(solvuu-build,nonstd)"
include _build/project.mk
_build/project.mk:
	$(OCAMLBUILD) $(notdir $@)

.PHONY: merlin
merlin:
	rm -f .merlin _build/.merlin && $(MAKE) .merlin && cat .merlin

