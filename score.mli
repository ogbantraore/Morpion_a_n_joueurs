exception Fin_partie of string
(*exception levee lorsque les conditions de victoire sont reunies pour un joueur x*)
val score_acc :
  string ->
  int ->
  int ->
  int ->
  int* int * string * string ->
  string array array -> int * int * string * string
(*fonction de scoring associee a eval_acc
   score_acc jo p x y acc g
   jo: le joueur pour lequel le score est calcule
   p: nombre de symbole a aligner pour gagner
   x,y: coordonnees de la case consideree dans la matrice
   acc: accumulateur du scoring (score,longueur,precedent,memoire) 
   g:matrice de caracteres representant la grille
   renvoie un accumulateur actualise par le traitement de la case (x,y) pour l'evaluation de la grille g*)
val score_acc2 :
  string ->
  int ->
  int ->
  int ->
  int * int * string * string ->
  string array array -> int * int * string * string
(*variante de la precedente fonction*)
val test_victoire : string array array -> int -> unit
(*test_victoire g p 
   g:matrice de caracteres representant la grille
   p: nombre de symboles a aligner pour gagner
   ne renvoie rien et en cas de victoire d'un joueur elle leve l'exception Fin_Partie*)

val eval_naif : string array array -> string -> int -> int -> int
val eval_acc : string array array -> string -> int -> int -> int
val eval_acc2 : string array array -> string -> int -> int -> int
val eval_str : string array array -> string -> int -> int -> int
val eval_str2 : string array array -> string -> int -> int -> int
val eval_str3 : string array array -> string -> int -> int -> int

(*fonctions d'evaluations
   eval_XXX g jo nb_jo p
   g: matrice de caractères representant la grille
   jo: joueur pour lequel est evalue la grille
   nb_jo: nombre de joueurs
   p: nombre de symboles a aligner pour gagner
   renvoie l'entier correspondant au score associe a la grille g pou jo
   En cas de defaite, score mis à -max_int et en cas de victoire à max_int*)

