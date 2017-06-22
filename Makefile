ExtendedStory: ExtendedStory.native
	cp $< $@

ExtendedStory.native: clean
	ocamlbuild -use-ocamlfind $@

clean:
	rm -rf _build ExtendedStory ExtendedStory.native
