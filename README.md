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

Nous avons implémenté une machine à pile qui gère les parties de fouine demandées : fonction récursives, aspects impératifs et exceptions. Nous avons aussi tenter d'ajouter les couples. En bonus, nous avons choisi de faire de l'inférence de types.



## Notre machine

Ses composants sont définis dans composantmachine. Nous réupérons l'arbre issu du parseur de fouine que nous compilons puis exécutons dans tradmachine. Enfin, nous avons créés plusieurs fonctions d'affichage dans showmachine, à la fois pour l'option stackcode et plus lobalement pour que s'affiche le code, la pile et l'environnement en cas de problème lors de l'exécution.

Notre langage à pile est défini de la façon suivante :

Les exceptions sont construites par :

Les couples ne marchent pas car :

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
- Debug intégral





