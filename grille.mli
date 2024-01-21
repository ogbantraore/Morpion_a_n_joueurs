exception Grille_pleine
(*exception levee quand aucun coup n'est possible*)
exception Coup_impossible
(*exception levee quand un joueur essaye de jouer sur une case deja remplie*)
type t
(*type grille*)
val creer_grille : int -> int -> t
(*creer_grille n m
   renvoie une grille vide de taille nxm*)
val coups : t -> (int*int) list
(*coups g renvoie la liste des coordonnees (i,j) des cases vides de g*)
val jouer : int -> int -> string -> t -> t
(*jouer i j x g renvoie la nouvelle grille g2 où x a ete joue en i j 
   si (i,j) n'est pas vide, levee de l'exception Coup_Impossible*)
val afficher_grille : t -> unit
(*affiche la grille du morpion dans le terminal*)
val generer_grille : int -> int -> int -> float -> t * string
(*generer_grille n m nb_jo t
    n,m: taille de la grille
    nb_jo: nombre de joueurs
    t: flottant représentant le taux de remplissage de la grille (0.=grille vide et  1.=grille pleine)
    renvoie le couple (g,x) où g est la grille generee et x le joueur qui doit jouer ensuite
   *)
val eval_naif_gr : t -> string -> int -> int -> int
val eval_acc_gr : t -> string -> int -> int -> int
val eval_acc2_gr : t -> string -> int -> int -> int
val test_victoire_gr : t -> int -> unit
val eval_str : t -> string -> int -> int -> int
val eval_str2 : t -> string -> int -> int -> int
val eval_str3 : t -> string -> int -> int -> int

(*application des fonctions du module score à une grille representee par le type grille*)
