.PHONY: build zip clean cleanall

build:
	dune build @install
	[ -e bin ] || ln -s _build/install/default/bin bin

zip: clean
	zip -9 -r sources.zip *

clean:
	dune clean
	rm -f bin sources.zip
	@printf 'Try `make cleanall` to remove also solutions.\n'

cleanall: clean
	rm -f solutions/*
