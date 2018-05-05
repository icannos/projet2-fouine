#!/bin/bash

# Exécute en parallèle sur Fouine et OCaml le script .ml passé en argument

ROOTPROJECT=$(dirname ${BASH_SOURCE[0]})

output_fouine=$( $ROOTPROJECT/bin/fouine $1 )

# On ajoute à la volée la définition de prInt pour qu' OCaml ne râle pas
output_ocaml=$(ocaml -w -A <(echo "let prInt x = print_int x;print_newline(); x;;" ; cat $1 ))

printf "Fouine: \n"
echo -e $output_fouine
printf "\n"

printf "=============================\n"

printf "Ocaml: \n"
echo -e $output_ocaml
printf "\n"
