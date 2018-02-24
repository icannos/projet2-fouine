#!/usr/bin/bash

#
# Ce script exécute parallèlement les scripts .ml avec fouine et ocaml puis effectue un diff des sorties
#

tests_dir="tests/"

echo -e "\n **************** Starting Tests *****************"
for test in $(ls -p $tests_dir | grep -v /)
do
	printf "Testing %-10s : %10s" $test
	printf "%10s \n"

		# On pipe les sorties des deux exécutions sur le diff
		# On ajoute à la volée la définition de prInt pour qu' OCaml ne râle pas
		output=$(diff <( ./bin/fouine $tests_dir/$test ) <( ocaml <(echo "let prInt x = print_int x;print_newline(); x;;" ; cat $tests_dir/$test) ) )

		# on affiche ok s'il n'y a pas de problèmes, sinon on affiche la sortie du diff.
		if [[ -z ${output//} ]]
		then
			echo -e "Ok."
		else
			echo -e $output
		fi

done;
