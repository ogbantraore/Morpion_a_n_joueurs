let maxi= fun (sa,ca) (sb,cb)-> if (sa>sb) then (sa,ca) else (sb,cb)

let minimax gr p jo a eva f=
 let eval =eva jo in
 let rec minimax_aux t b g x d=
  let js=string_of_int( (int_of_string(x) + 1) mod a) in
  try
    Grille.test_victoire_gr g p;
  if (d=f)||t=[] then ( let sc= eval g in sc ) else
    if b then 
      List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (min m (minimax_aux  l_prime (not b) g_prime js (d+1)))) (max_int) t
    else
      List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (max m (minimax_aux  l_prime (not b) g_prime js (d+1)))) (~-max_int) t
    with
    |Grille.Grille_pleine ->0
    |Score.Fin_partie v->if v=jo then max_int else  (~-max_int)
 
in
let js=string_of_int( (int_of_string(jo) + 1) mod a) and lt=Grille.coups gr in let res=(List.fold_left (fun m c->let i,j=c in let g_prime=Grille.jouer i j jo gr in maxi m ((minimax_aux (Grille.coups g_prime) true g_prime js 1),(i,j))) (~-max_int,(-1,-1)) lt) in 
snd res

let minimax_count gr p jo a eva f=
 let eval =eva jo in
 let nb_gr=ref 0 in
 let rec minimax_aux t b g x d=
 try
  Grille.test_victoire_gr g p;
  let js=string_of_int( (int_of_string(x) + 1) mod a) in
  if (d=f)||t=[] then (incr nb_gr; let sc= eval g in sc ) else
    if b then List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (min m (minimax_aux  l_prime (not b) g_prime js (d+1)))) (max_int) t
    else
      List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (max m (minimax_aux  l_prime (not b) g_prime js (d+1)))) (~-max_int) t
  with
  |Grille.Grille_pleine->incr nb_gr;0
  |Score.Fin_partie v ->incr nb_gr;if v=jo then max_int else (~-max_int)
in
let js=string_of_int( (int_of_string(jo) + 1) mod a) and lt=Grille.coups gr in
 (!nb_gr,snd (List.fold_left (fun m c->let i,j=c in let g_prime=Grille.jouer i j jo gr in maxi m ((minimax_aux (Grille.coups g_prime) true g_prime js 1),(i,j))) (~-max_int,(-1,-1)) lt))


 let alphabeta gr p jo a eva f  =
let eval =eva jo in
let rec alphabeta_aux t b g x d alpha beta=
try
 Grille.test_victoire_gr g p;
 let js=string_of_int( (int_of_string(x) + 1) mod a) in
 if (d=f)||t=[] 
   then ( let sc= eval g in sc ) 
 else
   if b then 
     List.fold_left 
       (fun m elt->let i,j=elt in 
       let g_prime=Grille.jouer i j x g in
        let l_prime=Grille.coups g_prime in
        if alpha >= m then m else (min m (alphabeta_aux  l_prime (not b) g_prime js (d+1) alpha m))
       ) (beta) t (* alpha -> - infini*)
   else
     List.fold_left 
     (fun m elt->let i,j=elt in 
     let g_prime=Grille.jouer i j x g in 
     let l_prime=Grille.coups g_prime in 
     if beta <=m then m 
     else 
     (max m (alphabeta_aux  l_prime (not b) g_prime js (d+1) m beta))) (alpha) t
 with
 |Grille.Grille_pleine->0
 |Score.Fin_partie v ->if v=jo then max_int else ~-max_int
in
let js=string_of_int( (int_of_string(jo) + 1) mod a) and lt=Grille.coups gr in
snd (
 List.fold_left 
 (fun m c->let i,j=c in 
 let g_prime=Grille.jouer i j jo gr in 
 maxi m ((alphabeta_aux (Grille.coups g_prime) true g_prime js 1 (~-max_int) max_int),(i,j))
 ) (~-max_int,(-1,-1)) lt)


 let alphabeta_count gr p jo a eva f  =
 let eval =eva jo in
 let nb_grille=ref 0 in
 let rec alphabeta_aux t b g x d alpha beta=
 try
  Grille.test_victoire_gr g p;
  let js=string_of_int( (int_of_string(x) + 1) mod a) in
  if (d=f)||t=[] 
    then (incr nb_grille; let sc= eval g in sc ) 
  else
    if b then 
      List.fold_left 
        (fun m elt->let i,j=elt in 
        let g_prime=Grille.jouer i j x g in
         let l_prime=Grille.coups g_prime in
         if alpha >= m then m else (min m (alphabeta_aux  l_prime (not b) g_prime js (d+1) alpha m))
        ) (beta) t (* alpha -> - infini*)
    else
      List.fold_left 
      (fun m elt->let i,j=elt in 
      let g_prime=Grille.jouer i j x g in 
      let l_prime=Grille.coups g_prime in 
      if beta <=m then m 
      else 
      (max m (alphabeta_aux  l_prime (not b) g_prime js (d+1) m beta))) (alpha) t
  with
  |Grille.Grille_pleine->incr nb_grille;0
  |Score.Fin_partie v ->incr nb_grille;if v=jo then max_int else ~-max_int
 in
 let js=string_of_int( (int_of_string(jo) + 1) mod a) and lt=Grille.coups gr in
 (!nb_grille,snd (
  List.fold_left 
  (fun m c->let i,j=c in 
  let g_prime=Grille.jouer i j jo gr in 
  maxi m ((alphabeta_aux (Grille.coups g_prime) true g_prime js 1 (~-max_int) max_int),(i,j))
  ) (~-max_int,(-1,-1)) lt))