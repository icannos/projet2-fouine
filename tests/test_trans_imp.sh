#!/bin/bash

#
# Ce script exécute parallèlement les scripts .ml avec fouine et ocaml puis effectue un diff des sorties
#

tests_dir="trans_imp"
fouinepath=../bin/fouine

echo -e "\n **************** Starting Tests *****************"
for test in $(ls -p $tests_dir | grep -E ".*\.ml"  )
do
	printf "Testing %-10s : %10s" $test
	printf "%10s \n"

		# On pipe les sorties des deux exécutions sur le diff
		# On ajoute à la volée la définition de prInt pour qu' OCaml ne râle pas
		$fouinepath -R $tests_dir/$test 



done;
