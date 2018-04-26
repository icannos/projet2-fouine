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

```

## Remarques -- Rendu 3

Ce rendu contient des exceptions testées, une traduction impérative et des continuations qui fonctionnent globalement. Cependant, nous avons eu de grandes difficultés sur cette dernière partie, et nous aimerions encore pouvoir bénéficier d'aide afin de mieux comprendre tout ce qui s'y passe.
Le travail de ce rendu a aussi consisté à reprendre selon les problèmes mentionnés au rendu 2 le code, et nous avons aussi pu corriger d'autres erreurs en implémentant ce qui était demandé.
Le rendu ne correspond donc pas à l'idéal dépeint par Boileau, il n'est certainement pas aussi peaufiné que cequ'il devrait l'être. Cependant, bien des choses ont été corrigé vingt fois et nous avons effacé souvent, et cette citation nous semble donc représentative de cet aspect besogneux.


## Remarques

- Pour les traductions, nous récupérons l'arbre sortant du parser et nous le transformons avant de l'évaluer. Notre système de gestion d'erreur n'est pas pas adapté à cette opération, donc nous perdons les numéros de lignes lors des traductions. De plus, pour avoir un code moins indigeste, nous avons créer des méta constructeurs, qui sont regroupés dans le fichier constructeur.
- Les traductions impératives sont dans tradimp, et ne traitent pas les bonus.
- Les continuations sont dans tradexcep. Les cas du bases sont fonctionnels, mais nous n'avons pas eu le temps de tester les bonus. De plus le cas des n-uplets nous a posé problème, mais doit au moins être juste pour le cas particulier des couples.
- Les différentes options demandées ont été implémentées. Nous avons ici ajouté un mode -m qui nous permet de regarder l'environnement et qui nous a aidé au débug.

## Tests

- Nous avions pour projet de refactoriser nos tests en proposant des corrections pour les traductions, mais le temps nous a manqué. Nous avons aussi eu la fâcheuse tendance à modifier tout le temps le même fichier, et nous sommes conscients que les tests que nous vous fournissons ne sont pas aussi nombreux qu'ils pourraient l'être, ni aussi systématiques que possibles
- Au niveau des tests que vous fournissez, plusieurs apparaissent faux à cause de l'erreur de parsing que vous nous avons déjà signalée. Cependant, nous les passons quitte à ajouter des espaces en début de ligne. Deux tests résistent néanmoins : basic-chaining à cause du g () qui n'est pas parser correctement et basic-exceptions car f x = raise (E x) (raise x à la place fonctionne). Nous n'avons pas eu le temps de nous plonger plus avant sur ces deux problèmes, que nous comptons cependant résoudre sous peu.

- Pour ce parseur, nous proposons, en restant polis :
	* ce gâcheur de temps 
	* ce trou noir de l'incompréhension
	* l'erreur invisible
	* .
.
.

## Répartition de travail.

Comme précédemment, nous insistons sur le fait que nous travaillons généralement en échangeant sur toutes les difficultés que nous rencontrons, voire nous travaillons autour de la même feuille et du même clavier.

Fait par Maxime
- Exceptions
- Suite des continuations
- Aide pour certains aspects des transformations.

Fait par Edwige
- Relecture des exceptions
- Transformations impératives
- Début des continuations
- Readme

Fait par Alain
- Thérapie de couple
- Aide à la compréhension de ces phénomènes étranges que constituent les transformations de programme

Fait par Totoro
- Debug intégral 





## Organisation pour les jours à venir

Pour Maxime
- faire les dossiers de tests  (pour dimanche)
- vérifier que tous les fichiers de tests des profs et d'Alain fonctionnent (idem)
- réfléchir (sur carnet donc dans un premier temps) au typage simple du 2.2 

Pour Edwige
- Faire un truc pour identifier les parties de fouine pur : choix d'annoatation : ajout d'un constructeur 'Pur' après le parseur et avant l'évaluation. -> La phrase Pour ce faire, vous devrez sans doute modifier légèrement le type qui sert à représenter les
programmes fouine du sujet me semble aller contre cette idée ? (Parce que j'imaginais juste remonter l'arbre) -> une étiquette supplémentaire partout ?
- Ajout du traitement de fouine pur :
	* faire la traduction caml assembleur
	* créer un environnemment et une pile
	* faire la fonction d'évaluation associée





## But :
d'ici jeudi prochain, le rendu intermédiaire est fini,le readme associé à cette partie également.


## Pour Maxime
On dirait que le truc de base fonctionne assez facilement, j'ai ajouté les options demandées et jai tout coder salement dans tradcomp, l'exemple de ce matin est dans exemplecomp et fonctionne. Je serais contente que tu me propose une réorganisation de ce que j'ai fait, et ce même si ça implique de réécrire une partie. J'ajoute les fonctions demain, mais j'ai des doutes sur la gestion de la clôture, si tu peux me donner ton avis avant c'est mieux. Cela rejoint le point suivant :

J'ai un gros doute sur la gestion de l'environnement eu égard au fait que Endlet ne prend pas d'argument mais ça me semble casse gueule. Pour l'instant j'ai implémenté ce qui correspondant le plus au slide, mais ça me semble incohérent.



















