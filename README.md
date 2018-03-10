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


#### Nouveau tour d'horizon

- env.ml : définition des environnements, des values et des varibles liées. On y gère les safe_op. Suggestion : séparer ce dernier cas en un autre fichier : il suffit d'y inclure env, donne moi ton accord et je le fais
- expr.ml : définie le type des expressions et leur évaluation et l'affichage. Suggestion : faire un fichier pour l'affichage et un pour l'évaluation.
- parser.mly : nos simplexpr et priexpr
- main.ml : ok
- errmgr.ml : pourquoi ce nom ? stocke le numéro de ligne pour les erreurs
- display.ml : définition de prInt et debug (ps = print_string)
- lexer.ml : correct sauf pour le ;; qui est trop simplifié en in.



Il serait intelligent d'utiliser freevars pour récupérer les variables libres de l'expression de la fonction

Changer bexpr et les opératuers binaires pour supprimer la redondance

#### Nouvelle vitrine
- affichage.ml : toutes les fonctions d'affichage, qui appellent donc Display
- expr.ml : ajout de bexpr et freevarsb sur la demande de Daniel
- eval.ml notre fonction d'évaluation, anciennement dans expr
- safe.ml regroupe tes fonctions qui évitent qu'on fasse n'importe quoi
- env.ml est devenu tout petit, et il faut y changer la définition de fonction.


#### Fonctions récursives

Les fonctions récursives fonctionnent maintenant très bien. Et les questions de priorités d'opérateurs semblent réglées. J'ai écrit la factorielle et la fonction d'ackermann en récursif et en utilisant plusieurs syntaxes. Tout semble fonctionner. Je pense qu'il nous faudrait nous concentrer sur la propreté du débuggage, des sorties et des arguments --verbose et de --debug. Ainsi que sur la gestion des erreurs avec de jolies exceptions bien rattrapées ! 




#### L'impératif

J'ai ajouté les refs, := et !, ça a l'air de fonctionner correctement, j'ai aussi implémenté la mémoire. J'ai fait 2 tests: ref et reffun, qui testent d'abord les refs d'entiers et ensuite les refs de fonctions.
J'ai commenté le code que j'avais modifié.

Par ailleurs: on utilise pas de Map au final mais des tableas de hashages de caml car les map ne possèdent pas de méthode pour récupérer facilement leur taille de plus les accès sont en log n ce qui est beaucoup au final quand on peut avoir avec un tableau dynamique un temps constant en amorti (voire ici un temps constant tout court car j'ai initialisé la table de hashage à 100 cases ce qui devrait amplement suffire).
Le truc chelou avec une struct dans Memory ça permet de définir un type init et les opérations de tests et de hashage qui lui sont associés. Ca sert à consruire la table de hashage. J'ai trouvé ça sur internet, on pourra en parler mais c'est pas compliqué.

Il y a un truc que je ne comprends pas: c'est la question du () qui est mentionné dans l'énoncé je ne comprends pas trop ce que l'on doit en faire: si ça permet de renvoyer des trucs de type unit dans le programme ou alors faire des fonctions qui ne prennent pas d'arguments... je veux bien ton avis là dessus.


#### Gestion des erreurs

J'ai implémenté un putain de système du turfu pour gérer les erreurs, c'est très moche et je suis persuadé qu'Hirshkoff nous donnera une meilleure solution mais actuellement je suis content de ma construction alors je laisse ça comme ça parce que ça marche assez bien. C'est pas encore fini mais j'ai construit le socle pour l'avenir !

















