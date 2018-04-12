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

> "Ce n'était qu'une fouine semblable à cent mille autres. Mais j'en ai fait mon amie, et elle est maintenant unique au monde." -- __Antoine de Saint Exupéry__ (adapté)

## Remarques -- Rendu intermédiaire

- Nous avons corrigés les erreurs/oublis signalés lors du précédent rendu.
- Nous avons aussi corrigé des erreurs non signalées. (Notamment concernant les constructeurs: on peut maintenant utiliser notamment des constructeurs sans argument.)
- Les exceptions sont terminées et même élargies: on peut lever n'importe quel constructeur via raise et le matcher avec dans le `try .. with` de la même manière que pour le pattern matching. Il est maintenant possible d'omettre le `|` lorsqu'il n'y a qu'un unique cas pour le pattern matching.
- Pour alléger la syntaxe, notamment pour la gestion des continuations nous avons implémenté la possibilité de définir des fonctions avec des constructeurs/couples en argument: `let f (a,b) = ... in` ou `fun (a,b) Constr(c) -> ...`
- Les traductions sont en cours.



### Etat des lieux sur la traduction des aspects impératifs.

- L'idée de départ est de gentiment transformer l'arbre fait  par le parseur en un autre arbre correspondant à la traduction. Pour faire ceci, on a utilisé une fonction d'affichage pour créer l'arbre correspondant à la transformation voulue.
- Cependant, cet essai infructueux met en avant de multiples problèmes via cette approche.
	* Le code n'est pas lisible : il faut clairement garder du code Caml (qui pour l'instant n'est que dans les fichiers traducmachin) Déjà illisble, ce sera tout bonnement impossible à faire pour les continuations)
	* Notre système d'erreur n'est pas du tout adapté à cette transformation, car les noeuds son doublement étiquetés : on perd lors de la traduction l'emplacement de l'erreur, et générer un arbre adéquat est folklorique.
	* Il serait judicieux de faire un renommage des variables pour ne pas avoir d'ambiguités avec alloc,... Par exemple en ajoutant un underscore au debut des noms des variables mais ce n'est pas le plus urgent.
- De nombreuses interrogations subsistent :
	* Est-ce globalement la bonne approche ?
	* Comment va-t-on faire le let rec ?
- Ces remarques nous indiquent la nécessité de faire :
	* Des couples qui peuvent être passés en argument (fait)
	* Des booléens moins stupides, qui regroupent les quatre opérateurs
	* Une fonction d'affichage du debug plus lisible (fait)
	* Une fonction qui concatène deux codes caml au début
	* Une fonction qui remplace un code caml par l'arbre correspondant
	* Une fonction qui remplace un Identifier (e1 pour ne pas le nommer) par l'arbre correspondant


### Pour le futur

On espère donc réussi à faire, pas à pas, tout ce qui est demandé pour le rendu avancé, pour peu que l'on nous renseigne un peu demain.
	
