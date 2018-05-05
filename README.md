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

> "Hâtez-vous lentement, et sans perdre courage,
Vingt fois sur le métier remettez votre ouvrage,
Polissez-le sans cesse, et le repolissez,
Ajoutez quelquefois, et souvent effacez." -- __Nicolas Boileau__



## Remarques -- Rendu 3

Ce rendu contient des exceptions testées, une traduction impérative et des continuations qui fonctionnent globalement. Cependant, nous avons eu de grandes difficultés sur cette dernière partie, et nous aimerions encore pouvoir bénéficier d'aide afin de mieux comprendre tout ce qui s'y passe.
Le travail de ce rendu a aussi consisté à reprendre selon les problèmes mentionnés au rendu 2 le code, et nous avons aussi pu corriger d'autres erreurs en implémentant ce qui était demandé.
Le rendu ne correspond donc pas à l'idéal dépeint par Boileau, il n'est certainement pas aussi peaufiné que cequ'il devrait l'être. Cependant, bien des choses ont été corrigé vingt fois et nous avons effacé souvent, et cette citation nous semble donc représentative de cet aspect besogneux.


## Remarques
- La compilation de fouine est maintenant Warning Free. Nous avons grandement amélioré la gestion des erreurs.
- Nous avons corrigé le bug lié aux retours à la ligne.
- Nous avons corrigé une partie importante des traductions. Un problème persiste cependant lorsque l'on effectue la traduction -ER car nos fonctions de gestion de la mémoire sont écrites en fouine et ne passent pas la traduction en continuation (question de pattern matching etc...). (Nous n'avions pas vu ce problème lors du rendu précédent)
- Nous avons aussi effectué un "fix" rapide pour coller aux consignes pour la syntaxe des exceptions: (E x).
- Les tests sont en cours de réorganisation dans tests. `machine` correspond aux tests de la machine à pile, `manual` aux tests nécessitant une correction manuelle, `simples` les tests pouvant être corrigés par OCaml, `trans_excep` les test pour la traduction -E et `trans_imp` ceux pour la traduction impérative. Des scripts bash pour chaque dossier sont fournis.

```
├── auto_test.sh
├── machine
│   ├── letrec.ml
│   └── printons.ml
├── manual
│   ├── arbre.ml
│   ├── couples.ml
│   ├── error_pattern.ml
│   ├── errors.ml
│   └── README.txt
├── simples ...
├── trans_excep
│   ├── div.ml
│   ├── imp.ml
│   ├── letrec.ml
│   └── README.txt
└── trans_imp
    ├── bertrand.ml
    ├── cond.ml
    ├── div.ml
    ├── function.ml
    ├── hard.ml
    ├── README.txt
    └── r_prof.ml
```


## Répartition de travail.

Comme précédemment, nous insistons sur le fait que nous travaillons généralement en échangeant sur toutes les difficultés que nous rencontrons, voire nous travaillons autour de la même feuille et du même clavier.

Fait par Maxime
- Exceptions
- Suite des continuations
- Aide pour certains aspects des transformations.
- Correction Rendu 3

Fait par Edwige
- Relecture des exceptions
- Transformations impératives
- Début des continuations
- Machine à pile (en cours)
- Readme

Fait par Alain
- Thérapie de couple
- Aide à la compréhension de ces phénomènes étranges que constituent les transformations de programme

Fait par Totoro
- Debug intégral





## Pour Maxime

Je continue à avoir un petit problème avec les arguments du print qui s'affichent deux fois. Si tu peux trouver d'où çça vient ça me réjouira.

Ajout des aspects impératifs :
On utilise la mémoire déjà utilisée de façon tradtionnelle :
- Pour créer une nouvelle référence, on execute le e, qui se met donc en haut de la pile, on lit un ref, donc on crée une nouvelle adresse à laquelle on ajoute la valeur de haut de la pile.
- Pour récupérer la valeur d'une ref





