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

Ce rendu contient des exceptions testées, une traduction impérative qui fonctionne également et des continuations qui laissent à désirer. Nous avons eu de grandes difficultés avec cette dernière partie.
Le travail de ce rendu a consisté à reprendre selon les problèmes mentionnés au rendu 2 le code, et nous avons aussi pu corriger d'autres erreurs en implémentant ce qui était demandé.

Le rendu ne correspond donc pas à l'idéal dépeint par Boileau, il n'est certainement pas aussi peaufiné que cequ'il devrait l'être. Cependant, bien des choses ont été corrigé vingt fois et nous avons effacé souvent, et cette citation nous semble donc représentative de cet aspect besogneux.

## Pour tester le code

Puisque les tests se sont fait plus variés pour ce rendu, nous avons jugé nécessaire de modifier notre fonctionnement de ce point de vue, d'où l'apparition d'un nouveau dossier test 2
l'idée est de ranger dans des dossiers différents les différents types de test et de créer différents bash chargés d'exécuter leurs dossiers respectifs. (Oui faudra faire ça ce soir).

## Remarques

- Le rendu intermédiaire fonctionne.
- Pour les traductions, nous récupérons l'arbre sortant du parser et nous le transformons avant de l'évaluer. Notre système de gestion d'erreur n'est pas pas adapté à cette opération, donc nous perdons les numéros de lignes lors des traductions. De plus, pour avoir un code moins indigeste, nous avons créer des méta constructeurs, qui sont regroupés dans le fichier constructeur.
- Les traductions impératives sont dans tradimp, et ne traitent pas les bonus.
- Les continuations sont dans tradexcep. Le let, le print, les opérations binaires et les conditions ont été testées et fonctionnent, contrairement au reste. Afin de vous permettre de lire ce que nous avons fait, nous avons ajouté un fichier traduction qui reprend sous forme lisible ce qui est implémenté.
- Les différentes options demandées ont été implémentées. Nous avons ici ajouté un mode -m qui nous permet de regarder l'environnement et qui nous a aidé au débug.

## Répartition de travail.

Fait par Maxime
- Exceptions
- Refonte des tests
- Aide pour certain aspects des transformations.

Fait par Edwige
- Transformations

Fait par Alain
- Thérapie de couple
- Aide à la compréhension de ces phénomènes étranges que constituent les transformations de programme

Fait par Totoro
- Debug intégral 

