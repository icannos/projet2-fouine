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

> Mal nommer un objet c'est ajouter au malheur de ce monde, car le mensonge est justement la grande misère humaine, c'est pourquoi la grande tâche humaine correspondante sera de ne pas servir le mensonge" -- __Albert Camus__

## Projet 2 - fouine

Bien que justement mis en intermédiaire par le classement initial nous avons été déplacé dans le groupe avancé après le premier rendu. Nous avons donc essayer de remplir les exigences attendues.

D'après les tests que nous avons pus faire, tout ce que nous avons implémenté fonctionne correctement. Notre rendu contient ce qui était attendu, ainsi que le matching, les types sommes et les listes.


## Pour tester le code

Le fichier auto_test effectue l'ensemble des tests et affiche 'Ok.' quand le résultat est identique à celui de Caml. Le fichier comp affiche la sortie Fouine et en dessous la sortie Caml. Ces deux fichiers ajoutent la fonciton `prInt` à la volée pour l'exécution avec OCaml.

## Remarques
- Les rendus intermédiaires et débutants fonctionnent.
- Sur les aspects impératifs : Le fichier memory.ml rassemble une implémentation naïve de la mémoire avec une table hashage qui relie les références à leur valeur. Nous notons que nous autorisons en fouine pour une référence de référence l'utilisation de !!x pour obtenir la valeur. Ce qui n'est pas autorisé en OCaml qui impose un espace entre les bangs.
- Sur la gestion des erreurs : Pour a gestion des erreurs on a étiqueté l'AST avec des entiers qui sont en fait un index dans table hashage, la fonction `error_handler ()` ajoute une entrée dans la table de hashage, y stocke les informations sur la position du noeud dans le fichier source et renvoie le numéro du noeud. Lorsque l'on rattrappe une erreur dans `eval` on l'envoie dans `error_display` qui s'occupe d'afficher toutes les informations nécessaires.
- Sur les couples : nous avons implémenter directement des n-uplets, en imposant n au moins égal à 2. Par ailleurs on impose aussi la présence de parenthèses autours de ceux-ci, contrairement à OCaml.
- Sur les types sommes : Les types sommes fonctionnent correctement et peuvent être matchés dans un `match .. with` ou dans un `let pattern = .. `, toujours sans typage. De même que pour les n-uplets, on impose des parenthèses pour les arguments passés aux constructeurs. Même s'il n'y en a qu'un.
- Sur les listes : nous avons implémenté les listes comme un peigne. Cependant, sans typage, les listes semblent perdre une grande partie de leur intérêt. L'avantage le plus notable a été de rajouter de nouveaux tests, notamment pour le pattern matching.

## A propos des tests

La plupart de nos tests fonctionnennt normalemnt (un ok est bien renvoyé etc...) mais dans certains cas puisque fouine n'est pas typé ocaml plante et donc nécessairement le diff des deux sorties n'est pas identique. Il y a quelques tests qui testent explicitement des erreurs. 

## Quelques détails sur les fichiers

- Dans affichage.ml : la fonction string_of_expr retourne la sortie exécutable en Caml. La fonction istring_of_expr est conçue pour faire apparaître plus explicitement le parsing.
- Dans safe.ml : on limite une partie des risques d'explosion lors de l'exécution en vérifiant les types pour les opérations arithmétiques.

```
.
├── auto_test.sh
├── comp.sh
├── README.md
├── src
│   ├── affichage.ml
│   ├── arguments.ml
│   ├── display.ml
│   ├── env.ml
│   ├── errmgr.ml
│   ├── eval.ml
│   ├── expr.ml
│   ├── lexer.mll
│   ├── main.ml
│   ├── Makefile
│   ├── memory.ml
│   ├── parser.mly
│   └── safe.ml
└── tests
    ├── ackermann2.ml
    ├── ackermann.ml
    ├── arbre.ml
    ├── arith.ml
    ├── comparaisons.ml
    ├── couples.ml
    ├── debug_test1.ml
    ├── error_pattern.ml
    ├── errors.ml
    ├── euclide.ml
    ├── facto.ml
    ├── fibo_prof.ml
    ├── functions2.ml
    ├── functions3.ml
    ├── functions_easy.ml
    ├── functions.ml
    ├── funrec.ml
    ├── fun_unit.ml
    ├── ifthenelse.ml
    ├── let3.ml
    ├── let4.ml
    ├── let_advanced.ml
    ├── let_var.ml
    ├── list_at.ml
    ├── liste-lasttwo.ml
    ├── liste_map.ml
    ├── liste_rev.ml
    ├── liste_test.ml
    ├── list_last.ml
    ├── merge_sort.ml
    ├── notype.ml
    ├── nuplets.ml
    ├── pattern_easy.ml
    ├── pattern_facto.ml
    ├── pgcd.ml
    ├── refernces_hard.ml
    ├── reffun.ml
    ├── ref_hard.ml
    ├── ref.ml
    └── unit.ml

```


### Répartition du travail

Conformément à votre demande, nous vous fournissons une répartition du travail. Cependant, nous avions juste envie de mettre « tout/2 » pour chacun. En effet, chaque partie a fait l’objet de débats entre nous, le code de l’un a été relu et débuggé par l’autre, les tests ont été écrits pendant que l’autre  finissait d’implémenter, ou à la relecture du code… Bref, plutôt qu’un simple découpage qui diviserait le travail par deux, nous pensons que travailler ensemble nous permet de réussir à faire ce qu’on ne réussirait pas seul avec deux fois plus de temps, grâce à nos deux approches, très différentes et terriblement complémentaires.

Fait par Maxime
- La gestion des erreurs
- Les aspects impératifs
- Les types sommes et matching
- Les fonctions
- La moitié des tests
- Apprendre le Caml à Edwige
- Dire ça marche alors que ça marche presque

Fait par Edwige
- Le contenu du rendu débutant
- La découpe du projet
- Le parser et le lexer
- Les listes
- L'affichage
- Le readme
- L’autre moitié des tests
- Supporter Maxime, détecter les "presque" et les supprimer
- Apporter une touche de sérieux au projet

Fait par Alain
- Thérapie de couple



### Pour Maxime :
- J'ai corrigé a priori toutes les erreurs du rendu 2 qu'on nous a signalées. J'ai fini d'écrire la fonction istring_of_expr, à part pour Raise, où je n'ai pas réussi à voir ce qui se passait.
- J'ai un peu modifié le parser pour les listes mais c'est du détail
- Il serait peut-être judicieux de changer nos booléens (regarde le cas Cond de trad_expr et tu comprendras)
- Le fichier traduction.ml transforme l'arbre sortant du parseur en l'arbre traduit sans faire appel aux aspects impératifs. C'est illisible et infaisable à la main, d'où l'apparition dans le dossier test de fichiers traducmachin qui correspondent à peu près à ce que j'ai mis.
Cependant et pour mon plus grand malheur, cette fonction ne compile pas : Manifestement, ma traduction de l'arbre n'est pas bonne (je me concentre sur le premier cas de matching pour l'instant) : J'ai un Identifier s0 qui lui deplaît, et sans identifier il est triste aussi : je ne comprends pas ce que doit être x dans Fun(x, expr) du coup...
- Cette traduction fait appel à trois fonctions qui agissent sur la mémoire. Celle-ci est une liste de couple (numéro, élément) pour avoir une implémentation fonctionnelle. Les trois fonctions sont dans memfonc.ml.
- Il est possible que je ne comprenne pas les apects impératifs : il faudra gérer ce problème apcr.
- Pour utiliser la traduction, ilfaut passer l'argument -R lors de l'exécution, que j'ai donc rajouté dans arguments.ml. Dans ce cas, il faudrait ajouter le chapeau memfonc.ml au code, et au lieu de passer le ast du parsing à eval, il faut le passer à trad_expr avec pour argument de l'immonde fonction résultante la liste vide et passer ensuite le tout à eval. (c'est ce que j'ai commencé à écrire dans le main, mais j'ai tout laissé en commentaire pour avoir un truc qui compile)


### Etat des lieux sur la traduction des aspects impératifs.

- L'idée de départ est de gentiment transformer l'arbre fait  par le parseur en un autre arbre correspondant à la traduction. Pour faire ceci, on a utilisé une fonction d'affichage pour créer l'arbre correspondant à la transformation voulue.
- Cependant, cet essai infructueux met en avant de multiples problèmes via cette approche.
	* Le code n'est pas lisible : il faut clairement garder du code Caml (qui pour l'nstant n'est que dans les fichiers traducmachin) Déjà illisble, ce sera tout bonnement impossible à faire pour les continuations
	* Notre système d'erreur n'est pas du tout adapté à cette transformation, car les noeuds son doublement étiquetés : on perd lors de la traduction l'emplacement de l'erreur, et générer un arbre adéquat est folklorique.
	* Il serait judicieux de faire un renommage des variables pour ne pas avoir d'ambiguités avec alloc,... PAr exemple en ajoutant un underscore au debut des noms des variables mais ce n'est pas le plus urgent.
- Ces remarques nous indiquent la nécessité de faire :
	* Des couples qui peuvent être passés en argument
	* Des booléens moins stupides, qui regroupent les quatre opérateurs
	* Une fonction d'affichage du debug plus lisible
	* Une fonction qui concatène deux codes caml au début
	* Une fonction qui remplace un code caml par l'arbre correspondant
	* Une fonction qui remplace un Identifier (e1 pour ne pas le nommer) par l'arbre correspondant
	* 



