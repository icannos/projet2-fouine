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


> "Cet intérêt, nous l'appelons passion losrque, refoulant tou autre intérêt ou but, l'individualité entière se projette sur un objectif avec toutes les fibres de son vouloir intérieur et concentre dans ce but tout ses forces et tous ses besoins. Rien de grand ne s'est accompli sans passion." -- __Hegel__


## Remarques -- rendu 4

Nous avons implémenté une machine à pile qui gère les parties de fouine demandées : fonction récursives, aspects impératifs et exceptions et les nuplets. En bonus, nous avons choisi de faire de l'inférence de types.

Notre rendu propose aussi une version corrigée des rendus précédents, et notamment sur les points problématiques des traductions.


## Notre machine

Ses composants sont définis dans composantmachine. Nous récupérons l'arbre issu du parseur de fouine que nous compilons puis exécutons dans tradmachine. Enfin, nous avons créé plusieurs fonctions d'affichage dans showmachine, à la fois pour l'option stackcode et plus globalement pour que s'affiche le code, la pile et l'environnement en cas de problème lors de l'exécution.

Notre langage à pile est défini comme dans le cours, avec les choix suivants pour les parties non traitées en cours :

- Les exceptions sont construites de la façon suivante. On ne gère que le cas des (E variable). Pour traduire try e1 with E x -> e2, on compile e1, puis on pose un jalon Beginwith lors duquel on regarde si on a récupère une exception lors de l'exécution de e1. Dans ce cas, on déclare celle-ci comme étant la valeur de la variable. Le endwith aurait donc pu être supprimé, mais nous l'avons d'une part pour nous aider à voir ladistinction entre deux étapes et d'autre part dans l'optique d'une possible généralisation de la portée des exceptions. Enfin, on execute e2 puis on supprime la variable que l'on a déclaré. Dans le cas contraire où aucune exception n'a été rencontrée on continue en ignorant le code jusqu'à Endexcep.

- Les aspects impératifs sont implémentés de façon très analogue à celle dans fouine. Nous avons changé de mémoire (cf `memmachine`) pour des problèmes de type,mais le reste fonctionne de la même manière.

- Les fonctions récursives sont définies par ClotR(f, x,code,env) Maxime just do that please

- Les couples sont implémentés de la façon suivante. On distingue les couples lors du let, et ceux utilisé avec des valeurs déjà définies. Dans le premier cas, on construit un Acoupler alors que sinon on définit un simple couple. Le fonctionnement est le suivant : on calcule progressivement toutes les valeurs du n-uplets sur la pile dans des Valcouple, puis quand le n-uplet est terminé on dépile jusqu'au début du couple. Ce fonctionnement ne marchait pas pour les déclarations de forme let (a,b) = (1,2) car il engendrait un Access à une variable qui devait au contraire être déclaré, d'où l'apparition de acoupler. Des solutions plus élégantes et plus efficaces sont sans aucun doute possible, par exemple pour permettre des déclarations enchâsser. Notre affichage des couples pourrait égalment être amélioré.

## L'inférence de type




## Remarques
- La compilation de fouine est maintenant Warning Free. Nous avons grandement amélioré la gestion des erreurs.
- Nous avons corrigé le bug lié aux retours à la ligne.
- Nous avons corrigé une partie importante des traductions. Un problème persiste cependant lorsque l'on effectue la traduction -ER car nos fonctions de gestion de la mémoire sont écrites en fouine et ne passent pas la traduction en continuation (question de pattern matching etc...). (Nous n'avions pas vu ce problème lors du rendu précédent)
- Nous avons aussi effectué un "fix" rapide pour coller aux consignes pour la syntaxe des exceptions: (E x).
- Les tests sont en cours de réorganisation dans tests. `machine` correspond aux tests de la machine à pile, `manual` aux tests nécessitant une correction manuelle, `simples` les tests pouvant être corrigés par OCaml, `trans_excep` les tests pour la traduction -E et `trans_imp` ceux pour la traduction impérative. Des scripts bash pour chaque dossier sont fournis.



## Répartition de travail.

Comme d'habitude, nous insistons sur le fait que nous travaillons généralement en échangeant sur toutes les difficultés que nous rencontrons, voire nous travaillons autour de la même feuille et du même clavier.

Fait par Maxime
- L'inférence de types

Fait par Edwige
- La machine

Fait par Alain
- Thérapie de couple

Fait par Totoro
- Debug





