MODEL=test
OBS=p
TIME=3600

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
	./ExtendedStory --max 5 -o tests/$(MODEL)/eoi -r $(OBS) tests/kasim_tmp/$(MODEL).json
	./dot.sh tests/$(MODEL)
	cd tests ; rm -r kasim_tmp