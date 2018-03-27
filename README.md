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

- Sur les aspects impératifs : Le fichier memory.ml rassemble une implémentation naïve de la mémoire avec une table hashage qui relie les références à leur valeur.
- Sur la gestion des erreurs : Pour a gestion des erreurs on a étiqueté l'AST avec des entiers qui sont en fait une case d'une table hashage, la fonction `error_handler ()` ajoute une entrée dans la table de hashage, y stocke les informations sur la position du noeud dans le fichier source et renvoie le numéro du noeud. Lorsque l'on rattrappe une erreur dans `eval` on l'envoie dans `error_display` qui s'occupe d'afficher toutes les informations nécessaires.
- Sur les couples : nous avons implémenter directement des n-uplets, en imposant n au moins égal à 2.
- Sur les types sommes : Les types sommes fonctionnent correctement et peuvent être matchés dans un `match .. with` ou dans un `let pattern = .. `, toujours sans typage.
- Sur les listes : nous avons implémenté les listes comme un peigne. Cependant, sans typage, les listes semblent perdre une grande partie de leur intérêt. L'avantage le plus notable a été de rajouter de nouveaux tests, notamment pour le pattern matching.

## Quelques détails sur les fichiers

- Dans affichage.ml : la fonction string_of_expr retourne la sortie exécutable en Caml. La fonction istring_of_expr est conçue pour faire apparaître plus explicitement le parsing.
- Dans safe.ml : on limite une partie des risques d'explosion lors de l'exécution en vérifiant les types pour les opérations arithmétiques.

```
├── auto_test.sh
├── bin
│   └── fouine
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
│   ├── main.native
│   ├── Makefile
│   ├── memory.ml
│   ├── parser.mly
│   └── safe.ml
|
└── tests
    ├── ackermann2.ml
    ├── ackermann.ml
    ├── arith.ml
    ├── comments.ml
    ├── comparaisons.ml
    ├── couples.ml
    ├── error_pattern.ml
    ├── errors.ml
    ├── euclide.ml
    ├── facto.ml
    ├── functions2.ml
    ├── functions3.ml
    ├── functions_easy.ml
    ├── functions.ml
    ├── funrec.ml
    ├── ifthenelse.ml
    ├── let_advanced.ml
    ├── let_var.ml
    ├── list_at.ml
    ├── liste-lasttwo.ml
    ├── liste_map.ml
    ├── liste_rev.ml
    ├── liste_test.ml
    ├── list_last.ml
    ├── merge_sort.ml
    ├── pattern_easy.ml
    ├── pattern_facto.ml
    ├── pgcd.ml
    ├── reffun.ml
    ├── ref_hard.ml
    └── ref.ml
```



### Répartition du travail

Conformément à votre demande, nous vous fournissons une répartition du travail. Cependant, nous avion juste envie de mettre « tout/2 » pour chacun. En effet, chaque partie à fait l’objet de débats entre nous, le code de l’un a été relu et débuggé par l’autre, les tests ont été écrits pendant que l’autre  finissait d’implémenter, ou à la relecture du code… Bref, plutôt qu’un simple découpage qui diviserait le travail par deux, nous pensons que travailler ensemble nous permet de réussir à faire ce qu’on ne réussirait pas seul avec deux fois plus de temps, grâce à nos deux approches, très différentes et terriblement complémentaires.

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




