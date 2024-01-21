let maxi= fun (sa,ca) (sb,cb)-> if (sa>sb) then (sa,ca) else (sb,cb)

let minimax gr p jo nb_jo eva f=
 let eval =eva jo in
 let rec minimax_aux t b g x d=
  let js=string_of_int( (int_of_string(x) + 1) mod nb_jo) in
  try
    Grille.test_victoire_gr g p;
  if (d=f)||t=[] then ( let sc= eval g in sc ) else
    if b then 
      List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (min m (minimax_aux  l_prime (js<>jo) g_prime js (d+1)))) (max_int) t
    else
      List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (max m (minimax_aux  l_prime (js<>jo) g_prime js (d+1)))) (~-max_int) t
    with
    |Grille.Grille_pleine ->0
    |Score.Fin_partie v->if v=jo then max_int else  (~-max_int)
 
in
let js=string_of_int( (int_of_string(jo) + 1) mod nb_jo) and lt=Grille.coups gr in
let rec meilleur_coup m t=match t with
  []->m
  |x::r->let i,j=x in 
  let g_prime=Grille.jouer i j jo gr in
  try
    Grille.test_victoire_gr g_prime p;
    let b_prime=maxi m ((minimax_aux (Grille.coups g_prime) true g_prime js 1 ),(i,j)) in meilleur_coup b_prime r
  with
  Score.Fin_partie _->(max_int,(i,j))
  |Grille.Grille_pleine->(0,(i,j)) 
  in
  snd(meilleur_coup (~-max_int,(-1,-1)) lt)

let minimax_count gr p jo nb_jo eva f=
 let eval =eva jo in
 let nb_gr=ref 0 in
 let rec minimax_aux t b g x d=
 try
  Grille.test_victoire_gr g p;
  let js=string_of_int( (int_of_string(x) + 1) mod nb_jo) in
  if (d=f)||t=[] then (incr nb_gr; let sc= eval g in sc ) else
    if b then List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (min m (minimax_aux  l_prime (js<>jo) g_prime js (d+1)))) (max_int) t
    else
      List.fold_left (fun m elt->let i,j=elt in let g_prime=Grille.jouer i j x g in let l_prime=Grille.coups g_prime in (max m (minimax_aux  l_prime (js<>jo) g_prime js (d+1)))) (~-max_int) t
  with
  |Grille.Grille_pleine->incr nb_gr;0
  |Score.Fin_partie v ->incr nb_gr;if v=jo then max_int else (~-max_int)
in
let js=string_of_int( (int_of_string(jo) + 1) mod nb_jo) and lt=Grille.coups gr in
let rec meilleur_coup m t=match t with
  []->m
  |x::r->let i,j=x in 
  let g_prime=Grille.jouer i j jo gr in
  try
    Grille.test_victoire_gr g_prime p;
    let b_prime=maxi m ((minimax_aux (Grille.coups g_prime) true g_prime js 1 ),(i,j)) in meilleur_coup b_prime r
  with
  Score.Fin_partie _->(max_int,(i,j))
  |Grille.Grille_pleine->(0,(i,j)) 
  in
  (!nb_gr,snd(meilleur_coup (~-max_int,(-1,-1)) lt))

let alphabeta gr p jo nb_jo eva f  =
let eval =eva jo in
let rec alphabeta_aux t b g x d alpha beta=
try
 Grille.test_victoire_gr g p;
 let js=string_of_int( (int_of_string(x) + 1) mod nb_jo) in
 if (d=f)||t=[] 
   then ( let sc= eval g in sc ) 
 else
   if b then 
     List.fold_left 
       (fun m elt->let i,j=elt in 
       let g_prime=Grille.jouer i j x g in
        let l_prime=Grille.coups g_prime in
        if alpha >= m then m else (min m (alphabeta_aux  l_prime  (js<>jo) g_prime js (d+1) alpha m))
       ) (beta) t (* alpha -> - infini*)
   else
     List.fold_left 
     (fun m elt->let i,j=elt in 
     let g_prime=Grille.jouer i j x g in 
     let l_prime=Grille.coups g_prime in 
     if beta <=m then m 
     else 
     (max m (alphabeta_aux  l_prime (js<>jo) g_prime js (d+1) m beta))) (alpha) t
 with
 |Grille.Grille_pleine->0
 |Score.Fin_partie v ->if v=jo then max_int else ~-max_int
in
let js=string_of_int( (int_of_string(jo) + 1) mod nb_jo) and lt=Grille.coups gr in
let rec meilleur_coup m t=match t with
  []->m
  |x::r->let i,j=x in 
  let g_prime=Grille.jouer i j jo gr in
  try
    Grille.test_victoire_gr g_prime p;
    let b_prime=maxi m ((alphabeta_aux (Grille.coups g_prime) true g_prime js 1 (~-max_int) max_int),(i,j)) in meilleur_coup b_prime r
  with
  Score.Fin_partie _->(max_int,(i,j))
  |Grille.Grille_pleine->(0,(i,j)) 
  in
  snd(meilleur_coup (~-max_int,(-1,-1)) lt)



 let alphabeta_count gr p jo nb_jo eva f  =
 let eval =eva jo in
 let nb_gr=ref 0 in
 let rec alphabeta_aux t b g x d alpha beta=
 try
  Grille.test_victoire_gr g p;
  let js=string_of_int( (int_of_string(x) + 1) mod nb_jo) in
  if (d=f)||t=[] 
    then (incr nb_gr; let sc= eval g in sc ) 
  else
    if b then 
      List.fold_left 
        (fun m elt->let i,j=elt in 
        let g_prime=Grille.jouer i j x g in
         let l_prime=Grille.coups g_prime in
         if alpha >= m then m else (min m (alphabeta_aux  l_prime (js<>jo) g_prime js (d+1) alpha m))
        ) (beta) t (* alpha -> - infini*)
    else
      List.fold_left 
      (fun m elt->let i,j=elt in 
      let g_prime=Grille.jouer i j x g in 
      let l_prime=Grille.coups g_prime in 
      if beta <=m then m 
      else 
      (max m (alphabeta_aux  l_prime (js<>jo) g_prime js (d+1) m beta))) (alpha) t
  with
  |Grille.Grille_pleine->incr nb_gr;0
  |Score.Fin_partie v ->incr nb_gr;if v=jo then max_int else ~-max_int
 in
 let js=string_of_int( (int_of_string(jo) + 1) mod nb_jo) and lt=Grille.coups gr in
 let rec meilleur_coup m t=match t with
  []->m
  |x::r->let i,j=x in 
  let g_prime=Grille.jouer i j jo gr in
  try
    Grille.test_victoire_gr g_prime p;
    let b_prime=maxi m ((alphabeta_aux (Grille.coups g_prime) true g_prime js 1 (~-max_int) max_int),(i,j)) in meilleur_coup b_prime r
  with
  Score.Fin_partie _->(max_int,(i,j))
  |Grille.Grille_pleine->(0,(i,j)) 
  in
  (!nb_gr,snd(meilleur_coup (~-max_int,(-1,-1)) lt))