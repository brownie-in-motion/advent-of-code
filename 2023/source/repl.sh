#!/bin/sh

# i.e. ./repl.sh bin/day_01.ml
# this is pretty bad, but it works
ocaml -init <( echo '#use_output "dune ocaml top";;' && cat "$@" )
