let nb_arguments=5

let lire_cp= fun x ->
  Printf.printf "Coup joueur %s ?" x;
  let line=read_line() in
    Scanf.sscanf line "%d %d" (fun i j->i,j)

let jouer_partie n m p nb_jo profond =
  let game=Grille.creer_grille n m in
  let rec coup k g=
  Grille.afficher_grille g; 
  try
  Grille.test_victoire_gr g p;
  let x=Sys.argv.(k+nb_arguments) and jo=string_of_int (k) and js= ((k+1) mod nb_jo) in
  if x="h" then  let i,j=lire_cp jo in coup js (Grille.jouer i j jo g);
  else
    if x="ma" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_narquois_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
    else 
      if x="ms" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
      else 
        if x="aa" then  let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_narquois_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
        else 
          if x="as" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in coup js (Grille.jouer i j jo g);
  with
  |Grille.Grille_pleine ->(Printf.printf "Nul\n")
  |Score.Fin_partie s->(Printf.printf "Victoire joueur %s!\n" s)
  |Grille.Coup_impossible->(Printf.printf "Coup invalide \n";coup k g )
  in coup 0 game


let comparaison algo1 algo2 n m p nb_jo profond=
  let game=Grille.creer_grille n m in
  let rec coup k g res=
  try
    Grille.test_victoire_gr g p;
    let jo=string_of_int(k) and js= ((k+1) mod nb_jo) in
    if jo="0" then let tps_deb=Sys.time() in let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (algo1 g) j nb_jo p) profond in let tps_fin=Sys.time() in (if nb=0 then (Grille.afficher_grille g) else ()); coup js (Grille.jouer i j jo g) (res@[(tps_fin-.tps_deb,nb)])
    else 
      let tps_deb=Sys.time() in let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (algo2 g) j nb_jo p) profond in let tps_fin=Sys.time() in (if nb=0 then (Grille.afficher_grille g) else ()); coup js (Grille.jouer i j jo g) (res@[(tps_fin-.tps_deb,nb)])
  with
  |Grille.Grille_pleine ->(Printf.printf "Nul\n"); res
  |Score.Fin_partie s->(Printf.printf "Victoire joueur %s!\n" s);res
  |Grille.Coup_impossible->(Printf.printf "Coup invalide \n";coup k g res)
  in 
  let l1=coup 0 game [] and l2=coup 1 game [] in
  List.iter2 (fun (ta,na) (tb,nb) -> Printf.printf "Partie 1 Temps: %.6f Nb de grilles: %7d   | Partie 2 Temps: %.6f Nb de grilles: %7d \n" ta na tb nb) l1 l2

let () =jouer_partie (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2))) (int_of_string(Sys.argv.(3)))  ((Array.length Sys.argv)-nb_arguments)  (int_of_string(Sys.argv.(4)))