# Morpion_a_n_joueurs
Developpement d'un jeu de Morpion à N joeurs en programmation fonctionnelle Ocaml

Pour lancer le programme vous devez installer ocaml sur votre ordinateur 

voici les commandes de compilations : 
ocamlopt -c score.ml ia.ml grille.ml
ocamlopt -o nom_de_l'exécutable score.cmx ia.cmx grille.cmx main.ml
cela vous générera l'exécutable que vous pourrez ensuite lancer avec la commande : 
./nom_de_l'exécutable nombre_de_ligne nombre_de_colonne nombre_de_symboles_a_aligner la_profondeur humain/machine humain/machine


