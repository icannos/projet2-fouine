```
 _____    _          _              _____                 ___  ___           _                 ______
|  ___|  | |        (_)            /  __ \       ___      |  \/  |          (_)                |  _  \
| |__  __| |_      ___  __ _  ___  | /  \/      ( _ )     | .  . | __ ___  ___ _ __ ___   ___  | | | |
|  __|/ _` \ \ /\ / / |/ _` |/ _ \ | |          / _ \/\   | |\/| |/ _` \ \/ / | '_ ` _ \ / _ \ | | | |
| |__| (_| |\ V  V /| | (_| |  __/ | \__/\_    | (_>  <   | |  | | (_| |>  <| | | | | | |  __/ | |/ /
\____/\__,_| \_/\_/ |_|\__, |\___|  \____(_)    \___/\/   \_|  |_/\__,_/_/\_\_|_| |_| |_|\___| |___(_)
                        __/ |
                       |___/
```

#### Pour Edwige

Tu trouveras dans le dossier `src/` les sources de notre programme ainsi qu'un makefile tuné. Lorsque tu lances make, il te crée un dossier `bin/` à la racine du projet dans lequel sont transférés les exécutables en adoptant la convention de nommage demandée dans le sujet (`main.native` --> `fouine`).

Dans le sujet il y a une note de bas de page à lire si on ne sait pas comment faire nous même: on sait faire, donc pas besoin d'en tenir compte (Ca parle de créer un fouine.sh etc... mais on fait plus proprement, nous, car on est LES MEILLEURS)


##### Références
La doc du lexeur
https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html

Pour l'ouverture d'un fichier comme flux (Ca sert à remplacer stdin pour le lexeur. open_in crée un truc de type in_channel)
https://ocaml.org/learn/tutorials/file_manipulation.html
https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#TYPEin_channel

Quelques explications sur le module qui gère les arguments passés en CLI (voir notamment les arguments anonymes ie sans mots clefs. On s'en sert pour passer le chemin du fichier source à lire)
http://scylardor.fr/2013/10/04/ocaml-parser-les-arguments-dun-programme-avec-le-module-arg/
http://caml.inria.fr/pub/docs/manual-ocaml-4.01/libref/Arg.html#TYPEkey


#### Scripts

- `./auto_test.sh` Effectuera l'ensemble des tests situés dans le dossier `tests/`. Il effectue un `diff` sur les sorties des scripts exécutés par `fouine` et par `ocaml` lui même. La définition de `prInt` est ajoutée à la volée lors du passage du fichier à OCaml.
- `./comp.sh` Permet d'exécuter un script .ml avec `OCaml` et `fouine`, de la même manière que pour `./auto_test.sh` la définition de `prInt` est ajoutée à la volée pour l'exécution par OCaml. Il affiche les 2 sorties de manière distincte.


#### Petit tour d'horizon
- pour le main : on a ajouté un fichier arguments pour gérer les options plus proprement. Par défaut srcfile vaut stdin, ce qui permet de garder la syntaxe habituelle possible :)
- pour le lexer : a priori pas de difficulté particulière, je pourrais toujours faire cette partie (donc j'ai mis plein de trucs qui ne sont pas faits ailleurs)
- pour le parseur : il faut construire l'arbre à partir du résultat du lexeur.
- pour expr : c'est là qu'on utilise l'arbre qu'on a construit. Contient les définitions des value et des expr ainsi que les fonctions d'évaluations et d'affichage : ne serait-ce pas judicieux de scinder les deux ?


#### J'ai fait 2-3 trucs

- Un peu de réorganisation (rien de fou)
- J'ai ajouté une fonction debug dans laquelle on fera ce qu'il faut en fonction du niveau de débuggage voulu, l'idée c'est qu'on la met tout le temps, partout
- On gère maintenant les let _ = ... in/;;
- J'ai écrit quelques tests qui fonctionnent correctement
- J'ai fait la fonction d'affiche de l'environnement
- A l'avenir il faudra certainement réécrire les opérateurs arithmétiques: puisque eval renverra un truc de type value ie int ou cloture, d'ailleurs on devrait quoi qu'il arrive faire en sorte qu'eval renvoie une cloture (int, env) ou (fun, env): cela permettra de gérer les séquencements plus efficacement je pense
- Ce serait aussi pas mal d'écrire des fonctions pour chaque cas du matching au lieu d'avoir le code directement dedans
- J'ai réfléchi à la méthode pour faire les fonctions et les fonctions récursives: ça va bien se passer ;)
