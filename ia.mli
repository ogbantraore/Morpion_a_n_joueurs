val minimax :
  Grille.t ->
  int -> string -> int -> (string -> Grille.t -> int) -> int -> int * int
(*minimax gr p jo nb_jo eva f
   gr: une grille de morpion (type grille)
   p: nombre de symboles a aligner pour gagner
   jo: joueur pour lequel minimax est evalue
   nb_jo: nombre de joueurs
   eva: fonction d'evaluation
   f: profondeur d'exploration
   renvoie le couple (i,j) correspondant au premier coup de jo en appliquant le minimax aux grilles avec eva*)
val minimax_count :
  Grille.t ->
  int ->
  string -> int -> (string -> Grille.t -> int) -> int -> int * (int * int)
  (* fonction identique à la precedente mais renvoie en plus du coup le nombre de feuilles explorees dans l'arbre des grilles possibles*)
val alphabeta :
  Grille.t ->
  int -> string -> int -> (string -> Grille.t -> int) -> int -> int * int
  (*fonction similaire à minimax mais le couple (i,j) renvoye est fruit de l'application de l'alphabeta avec eva*)
val alphabeta_count :
  Grille.t ->
  int ->
  string -> int -> (string -> Grille.t -> int) -> int -> int * (int * int)
  (*fonction identique à alphabeta mais renvoie en plus du coup le nombre de feuilles explorees dans l'arbre des grilles possibles*)
