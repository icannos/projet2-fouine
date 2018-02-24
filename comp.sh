#!/usr/bin/bash

# Exécute en parallèle sur Fouine et OCaml le script .ml passé en argument

output_fouine=$( ./bin/fouine $1 )

# On ajoute à la volé la définition de prInt pour qu' OCaml ne râle pas
output_ocaml=$(ocaml <(echo "let prInt x = print_int x;print_newline(); x;;" ; cat $1 ))

printf "Fouine: \n"
echo -e $output_fouine
printf "\n"

printf "=============================\n"

printf "Ocaml: \n"
echo -e $output_ocaml
printf "\n"
