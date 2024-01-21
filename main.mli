val nb_arguments : int
(*nombre d'arguments a entrer en ligne de commande pour executer la fonction dans le main
   5 pour lancer une partie
   6 pour la comparaison
   7 pour les statistiques*)
val lire_cp : string -> int * int
(*lire_cp x 
   x: symbole du joueur pour lequel on veut lire le coup
   renvoie les coordonnées de la case (i,j) lues dans l'entrée standard*)

val jouer_partie : int -> int -> int -> int -> int -> unit
(*jouer_partie n m p nb_jo profond
   n,m: taille de la grille
   p: nombre de symboles à aligner pour gagner
   nb_jo: nombre de joueurs
   profond: profondeur d'exploration pour les IA
   affiche dans le terminal l'issue de la partie: Victoire du joueur i ou Nul
   Pour lancer une partie, verifier que cette fonction est appelee dans le main et que nb_arguments=5 
   avec les paramètres en ligne de commande associés (sauf nb_jo qui est calculé automatiquement) suivi des codes des joueurs (voir rapport)*)

val affichage_comparaison :
  (float * int * int) list -> (float * int * int) list -> unit
  (*affichage_comparaison l1 l2
  l1,l2: listes des coups à l'issue des deux parties de comparaison
  affiche dans le terminal le temps d'execution et le nombre de grilles explorees pour chaque coup
     *)

val comparaison : int -> int -> int -> int -> int -> float -> unit
(*comparaison n m p nb_jo profond t
   n,m: taille de la grille
   p: nombre de symboles a aligner pour gagner
   nb_jo: nombre de joueurs
   profond: profondeur d'exploration pour les IA
   t: flottant représentant le taux de remplissage de la grille (0.=grille vide et  1.=grille pleine)
   construit 2 listes de coups (tuple (temps d'execution,nombre de grilles explorees,joueur)) correspondants aux 2 parties de comparaison (voir rapport)
   La grille initiale est generee avec la fonction generer_grille et les parametres n m nb_jo t.
   Pour lancer une comparaison, verifier que cette fonction est appelee dans le main et que nb_arguments=6 
   avec les paramètres en ligne de commande associés (sauf nb_jo qui est calculé automatiquement) suivi des codes des joueurs (voir rapport)*)
val statistiques : int -> int -> int -> int -> int -> float -> int -> unit
(*statistiques n m p nb_jo profond t s
   n,m: taille de la grille
   p: nombre de symboles a aligner pour gagner
   nb_jo: nombre de joueurs
   profond: profondeur d'exploration pour les IA
   t: flottant représentant le taux de remplissage de la grille (0.=grille vide et  1.=grille pleine)
   s: nombre de grilles generees
   Lance successivement s parties en commençant par le premier algorithme puis le second.
   Les grilles initiales sont generees avec la fonction generer_grille et les parametres n m nb_jo t. Ce sont les mêmes grilles pour les 2 joueurs.
   Pour lancer une statistique, verifier que cette fonction est appelee dans le main et que nb_arguments=7
   avec les paramètres en ligne de commande associés (sauf nb_jo qui est calculé automatiquement) suivi des codes des joueurs (voir rapport)*)