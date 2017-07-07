MODEL=test
OBS=p
TIME=10

ExtendedStory: ExtendedStory.native
	cp $< $@

ExtendedStory.native: clean
	ocamlbuild -use-ocamlfind $@

clean:
	rm -rf _build ExtendedStory ExtendedStory.native

test:
	cd tests ; KaSim -i $(MODEL).ka -l $(TIME) -trace $(MODEL).json
	mkdir -p tests/$(MODEL)
	./ExtendedStory -o tests/$(MODEL)/eoi -r $(OBS) tests/$(MODEL).json
	cd tests ; ./dot.sh $(MODEL)
	cd tests ; rm $(MODEL).json inputs*.ka