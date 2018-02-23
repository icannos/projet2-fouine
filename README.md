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

#### Pour Edwige

Tu trouveras dans le dossier `src/` les sources de notre programme ainsi qu'un makefile tuné. Lorsque tu lances make, il te crée un dossier `bin/` à la racine du projet dans lequel sont transférés les exécutables en adoptant la convention de nommage demandée dans le sujet (`main.native` --> `fouine`).

Dans le sujet il y a une note de bas de page à lire si on ne sait pas comment faire nous même: on sait faire, donc pas besoin d'en tenir compte (Ca parle de créer un fouine.sh etc... mais on fait plus proprement, nous, car on est LES MEILLEURS)


##### Références
La doc du lexeur
https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html

Pour l'ouverture d'un fichier comme flux (Ca sert à remplacer stdin pour le lexeur. open_in crée un truc de type in_channel)
https://ocaml.org/learn/tutorials/file_manipulation.html
https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#TYPEin_channel

Quelques explications sur le module qui gère les arguments passés en CLI (voir notamment les arguments anonymes ie sans mots clefs. On s'en sert pour passer le chemin du fichier source à lire)
http://scylardor.fr/2013/10/04/ocaml-parser-les-arguments-dun-programme-avec-le-module-arg/
http://caml.inria.fr/pub/docs/manual-ocaml-4.01/libref/Arg.html#TYPEkey
