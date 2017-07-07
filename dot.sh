#!/bin/sh

for d in $1/*.dot
do
    dot -Tpdf $d > $d.pdf
done
