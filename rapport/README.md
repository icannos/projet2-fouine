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


> "Un je ne sais quoi" -- __Jankélévitch__


## Organisation du projet

.
├── rapport
│   ├── biblio.bib
│   ├── presentation.pdf
│   ├── rapport.pdf
│   └── README.md
├── README.md
├── src
│   ├── affichage.ml : gère l'affichage de fouine.
│   ├── arguments.ml : gère l'ensemble des arguments que l'on peut passer à fouine
│   ├── composantmachine.ml : déclaration des différentes parties de notre machine
│   ├── constructeur.ml : définition de nos méta constructeurs pour les transformations
│   ├── display.ml : fonctions auxiliaires pour l'affichage
│   ├── env.ml : l'environnement de fouine
│   ├── errmgr.ml : la gestion des erreurs
│   ├── eval.ml : l'évaluation de l'arbre en fouine
│   ├── expr.ml : définition des expressions fouine
│   ├── lexer.mll
│   ├── main.ml
│   ├── main.native
│   ├── Makefile
│   ├── memfonc.ml : une mémoire totalement fonctionnelle pour les continuations
│   ├── memmachine.ml : une mémoire similaire à celle de fouine pour notre machine
│   ├── memory.ml : la mémoire de fouine
│   ├── parser.mly
│   ├── safe.ml : vérifie la compatibilté ddes opérateurs
│   ├── showmachine.ml : l'affichage de la machine
│   ├── tradcont.ml : la partie traduction des continuations
│   ├── tradimp.ml : la partie traductions des aspects impératifs
│   ├── tradmachine.ml : la compilation et l'exécution de notre machine
│   └── typechecking.ml : l'inférence de type
├── tests
│   ├── machine
│   ├── manual
│   │   └── README.txt
│   ├── simples
│   ├── test_simples.sh
│   ├── test_trans_er.sh
│   ├── test_trans_excep.sh
│   ├── test_trans_imp.sh
│   ├── test_trans_re.sh
│   ├── trans_excep
│   ├── trans_imp
│   └── typechecking

## L'interpréteur fouine

Notre interpréteur est à notre connaissance parfaitement fonctionnel. Il comporte des aspects impératifs reposant sur une mémoire sous forme de table de hashage. Nous gérons les exceptions sous la forme demandée, mais aussi dans un cadre plus général d'un constructeur quelconque.

Plus généralement, nous avons traité les couples dans le cas des n-uplets, mais nous imposons les parenthèses autour de ceux-ci. Nous avons fait le matching et les types sommes,ainsi que les listes. L'affichage a été amélioré progressivement.

## Les traductions

Pour gérer les continuations, nous récupérons l'arbre issu du parseur que nous transformons en un autre arbre grâce à des méta constructeurs. Actuellement, les transformations -R -E et -RE fonctionnent sur tous les attendus. Cependant la traduction -ER résiste car les fonctions de mémoire ne semblent pas être appelées correctement.

## La gestion des erreurs

Nous avons implémenté un système d'erreurs qui affiche précisément où l'erreur a lieu. Pour cela, nous avons étiqueté notre arbre de parseur par des noeuds, que nous récupérons en cas de problème lors de la fonction eval. Ce système a permis de faire un joli affichage pour notre interpréteur, cependant nous avons du renoncé à conserver des noeuds cohérents lors des transformations de programme.
Pour la machine, nous avons aussi une gestion élémentaire : en cas de problème au cours de l'exécution, on affiche qu'on a eu un problème, puis on affiche le code restant, les noms des éléments présents dans l'environnement et ce qu'il y a dans la pile. 


## La machine

Plus de détails dans le readme spécifique au rendu

Ses composants sont définis dans composantmachine. Nous récupérons l'arbre issu du parseur de fouine que nous compilons puis exécutons dans tradmachine. Enfin, nous avons créé plusieurs fonctions d'affichage dans showmachine, à la fois pour l'option stackcode et plus globalement pour que s'affiche le code, la pile et l'environnement en cas de problème lors de l'exécution.

- Pour les exeptions, on ne gère que le cas des (E variable).
- Les aspects impératifs sont implémentés de façon très analogue à celle dans fouine
- Les fonctions récursives sont définies par ClotR(f, x,code,env) Maxime just do that please
- Les couples sont implémentés.
- Au niveau de l'affichage : on affiche comme demandé le haut de la pile à la fin et les print en cours d'exécution. Si le haut de la pile est un uplet, on affiche les éléments qui sont des nombres, et NAN sinon.

La suite logique, que nous aurions aimé traiter avec plus de temps était de rajouter les indices de Brujin, mais l'extension aux transformations et au couple s'est déjà révélée suffisamment chronophage.

## L'inférence de type



## Les tests

Nos tests sont regroupés par sous dossier selon la partie de code qu'ils testent. Ils possèdent des scripts associés qui les exécutent automatiquement et les comparent à la sortie fournies par Caml.






