# ExtendedStory

Causal analysis for Kappa. In its current state, 
this program does the following for each occurence of the rule of interest:
 * Compute the causal core of the event of interest using KaFlow (note that any other compression engine could also be used).
 * Enrich the core with counterfactual experiments and inhibition arrows by using the Kappa resimulator and some heuristics.
 * Output the result in a dot file.

## How to build

You need KaSim to use this tool. To install it, simply type : `opam install KaSim`.

Then, to build this project, simply type `make`.

## How to test

You can test this project on the examples in the *tests* folder, or on your own Kappa model.

To test it on your own model, simply put it in the *tests* folder and type :
`make test MODEL=your_model OBS=rule_of_interest TIME=simulation_time`.

You can also directly use the ExtendedStory binary on a trace if you need more options.

To run our examples :
```
make test
make test MODEL=test2 OBS=BAC
make test MODEL=test3 OBS=Aphos
make test MODEL=test4 OBS=AB
```

## How to render stories

For the .json format (default) : load it with story_viewer/story_viewer.html.

You can find some json examples in tests/examples.

For the .dot format : use graphviz to render it.
