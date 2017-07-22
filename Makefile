MODEL=test
OBS=Sp
TIME=3600
CONFIG=regular
MAX=5

ExtendedStory: ExtendedStory.native
	cp $< $@

ExtendedStory.native: clean
	ocamlbuild -use-ocamlfind $@

clean:
	rm -rf _build ExtendedStory ExtendedStory.native

test:
	mkdir -p tests/kasim_tmp
	cd tests/kasim_tmp ; KaSim ../$(MODEL).ka -l $(TIME) -trace $(MODEL).json
	mkdir -p tests/$(MODEL)
	./ExtendedStory --max $(MAX) -o tests/$(MODEL)/eoi -r $(OBS) -c $(CONFIG) tests/kasim_tmp/$(MODEL).json
	cd tests ; rm -r kasim_tmp

test_dot:
	mkdir -p tests/kasim_tmp
	cd tests/kasim_tmp ; KaSim ../$(MODEL).ka -l $(TIME) -trace $(MODEL).json
	mkdir -p tests/$(MODEL)
	./ExtendedStory --dot --max $(MAX) -o tests/$(MODEL)/eoi -r $(OBS) -c $(CONFIG) tests/kasim_tmp/$(MODEL).json
	./dot.sh tests/$(MODEL)
	cd tests ; rm -r kasim_tmp