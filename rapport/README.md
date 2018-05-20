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

## Documentation

L'intégralité du projet Fouine est documenté dans le dossier `doc/`. La documentation est générée lors de la compilation du projet et disponible en html et en PDF dans les dossiers correspondants. Toutes les fonctions sont commentées et décrites dans les fichiers sources et donc dans la documentation.

Des versions compilées de notre rapport et de notre présentation se trouvent dans le dossier `pdf/` à la racine du projet.

## Organisation du projet
```
.
├── rapport
│   ├── biblio.bib
│   ├── presentation.pdf
│   ├── rapport.pdf
│   └── README.md
├── README.md
├── doc Contient la documentation de notre projet
├── pdf Contient les versions compilées de notre rapport et de notre présentation
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
```
## L'interpréteur fouine

Notre interpréteur est à notre connaissance parfaitement fonctionnel. Il comporte des aspects impératifs reposant sur une mémoire sous forme de table de hashage. Nous gérons les exceptions sous la forme demandée, mais aussi dans un cadre plus général d'un constructeur quelconque.

Plus généralement, nous avons traité les couples dans le cas des n-uplets, mais nous imposons les parenthèses autour de ceux-ci. Nous avons fait le matching et les types sommes, ainsi que les listes. L'affichage a été amélioré progressivement.

## Les traductions

Pour gérer les continuations, nous récupérons l'arbre issu du parseur que nous transformons en un autre arbre grâce à des méta constructeurs. Actuellement, les transformations -R -E et -RE fonctionnent sur tous les attendus. Cependant la traduction -RE résiste car les fonctions de mémoire ne semblent pas être appelées correctement.

## La gestion des erreurs

Nous avons implémenté un système d'erreurs qui affiche précisément où l'erreur a lieu. Pour cela, nous avons étiqueté notre arbre de parseur par des noeuds, que nous récupérons en cas de problème lors de la fonction eval. Ce système a permis de faire un joli affichage pour notre interpréteur, cependant nous avons du renoncé à conserver des noeuds cohérents lors des transformations de programme.
Pour la machine, nous avons aussi une gestion élémentaire : en cas de problème au cours de l'exécution, on affiche qu'on a eu un problème, puis on affiche le code restant, les noms des éléments présents dans l'environnement et ce qu'il y a dans la pile.

L'inférence de type utilise ce mécanisme pour signaler l'emplacement des erreurs de typage.

## La machine

Plus de détails dans le readme spécifique au rendu

Ses composants sont définis dans composantmachine. Nous récupérons l'arbre issu du parseur de fouine que nous compilons puis exécutons dans tradmachine. Enfin, nous avons créé plusieurs fonctions d'affichage dans showmachine, à la fois pour l'option stackcode et plus globalement pour que s'affiche le code, la pile et l'environnement en cas de problème lors de l'exécution.

- Pour les exeptions, on ne gère que le cas des (E variable).
- Les aspects impératifs sont implémentés de façon très analogue à celle dans fouine
- Les fonctions récursives sont définies par ClotR(f, x,code,env) Pour traduire un `let rec` on évalue d'abord le contenu que l'on place sur la pile (qui est une cloture d'une fonction classique), puis la commande `Rec f` construit une clôture récursive à partir de la cloture sur la pile. De plus, de la même manière que dans l'interpréteur. Une clôture récursive conserve le nom de la fonction, l'argument, le code et l'état de la pile lors de sa création.
- Les couples sont implémentés.
- Au niveau de l'affichage : on affiche les print en cours d'exécution. Nous avons laissé en commentaire dans tradmachine la possibilité d'afficher le haut de la pile en fin d'exécution. Si le haut de la pile est un uplet, on affiche les éléments qui sont des nombres, et NAN sinon.

La suite logique, que nous aurions aimé traiter avec plus de temps était de rajouter les indices de Brujin, mais l'extension aux transformations et au couple s'est déjà révélée suffisamment chronophage.

## L'inférence de type
Nous gérons l'inférence de type dans le fichier `typechecking.ml`. L'inférence de type correspond à un union find pour déterminer les classes d'équivalences des différentes variables et à un algorithme d'unification qui essaie de déterminer si deux éléments peuvent avoir le même type.

Pour plus d'informations voir le ReadMe du Rendu 4.

## Bonus traités

Nous avons traité un certain nombre des bonus proposés tout au long du projet et nous avons essayé de les maintenir malgré la difficulté croissante. Pour exemple l'inférence de type gère les listes et les n-uplets pas les types sommes. C'était prévu mais le temps nous a fait défaut.

* n-uplets (avec une limitation syntaxique: on impose des parenthèses autours systématiquement)
* Les type sommes (même limitation)
* Le pattern Matching (uniquement le match ... with et sans les when)
* Les listes
* L'inférence de type (Avec une gestion simple du polymorphisme)

## Les tests

Nos tests sont regroupés par sous dossier selon la partie de code qu'ils testent. Ils possèdent des scripts associés qui les exécutent automatiquement et les comparent à la sortie fournies par Caml ou à la sortie de fouine dans le cas des traductions.

## Ce qu'il resterait à faire

Nous avons posé les bases de l'ajout de types utilisateurs et de leur inférence. Le parsing de la commande `type name = ...` a été réalisé mais nous n'avons pas eu le temps de terminer.
Nous aurions aussi voulu terminer l'implémentation des transformations de programmes pour prendre en compte le pattern maching et les types sommes.
