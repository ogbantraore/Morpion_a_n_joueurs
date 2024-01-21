let nb_arguments=7

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
    if x="mn" then  let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_naif_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
    else
      if x="ma" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_acc_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
      else
        if x="ma2" then  let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_acc2_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
        else 
          if x="ms" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
          else
            if x="ms2" then  let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_str2 g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
            else
              if x="an" then  let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_naif_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
              else
                if x="aa" then  let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_acc_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
                else
                  if x="aa2" then  let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_acc2_gr g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
                  else
                    if x="as" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
                    else
                      if x="as2" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_str2 g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
                      else
                        let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_str3 g) j nb_jo p) profond in coup js (Grille.jouer i j jo g)
  with
  |Score.Fin_partie s->(Printf.printf "Victoire joueur %s!\n" s)
  |Grille.Coup_impossible->(Printf.printf "Coup invalide \n";coup k g )
  |Grille.Grille_pleine->(Printf.printf "Nul \n");
  in coup 0 game


  let affichage_comparaison l1 l2=
  let rec parc_lis a b=match a,b with
  |[],[]->()
  |[],(tb,nb,k)::t-> Printf.printf "Partie 1 Joueur:   Temps: %.6f Nb de grilles: %8d   | Partie 2 Joueur: %d Temps: %.6f Nb de grilles: %8d \n" 0. 0 k tb nb; 
  |(ta,na,k)::y,[]->Printf.printf "Partie 1 Joueur: %d Temps: %.6f Nb de grilles: %8d   | Partie 2 Joueur:   Temps: %.6f Nb de grilles: %8d \n" k ta na 0. 0; parc_lis y []
  |(ta,na,k)::y,(tb,nb,j)::t->Printf.printf "Partie 1 Joueur: %d Temps: %.6f Nb de grilles: %8d   | Partie 2 Joueur: %d Temps: %.6f Nb de grilles: %8d \n" k ta na j tb nb; parc_lis y t
in parc_lis l1 l2


let generer_grille n m nb_jo t=
let g=Grille.creer_grille n m in
let nb_coups=(int_of_float (float_of_int(n*m)*.t)/2)*2 in
let rec remplir = fun k x g->
if k=0 then g,x
else
let lc=Grille.coups g in
let len=List.length lc in
let (i,j)=List.nth lc (Random.int len) in
remplir (k-1) (string_of_int( ((int_of_string x)+1) mod nb_jo)) (Grille.jouer i j x g) 
in
remplir nb_coups "0" g

let comparaison n m p nb_jo profond t=
  let game,jc=Grille.generer_grille n m nb_jo t in
  assert (nb_jo = 2);
  let rec coup k g res=
  try
    Grille.test_victoire_gr g p;
    let x=Sys.argv.(k+nb_arguments) and jo=string_of_int (k) and js= ((k+1) mod nb_jo) in
    if x="mn" then let tps_deb=Sys.time() in  let (nb,(i,j))=Ia.minimax_count g p jo nb_jo (fun j g-> (Grille.eval_naif_gr g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
    else
      if x="ma" then let tps_deb=Sys.time() in let (nb,(i,j))=Ia.minimax_count g p jo nb_jo (fun j g-> (Grille.eval_acc_gr g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
      else
        if x="ma2" then let tps_deb=Sys.time() in  let (nb,(i,j))=Ia.minimax_count g p jo nb_jo (fun j g-> (Grille.eval_acc2_gr g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
        else 
          if x="ms" then let tps_deb=Sys.time() in let (nb,(i,j))=Ia.minimax_count g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
          else
            if x="ms2" then let tps_deb=Sys.time() in  let (nb,(i,j))=Ia.minimax_count g p jo nb_jo (fun j g-> (Grille.eval_str2 g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
            else
              if x="an" then let tps_deb=Sys.time() in  let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (Grille.eval_naif_gr g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
              else
                if x="aa" then let tps_deb=Sys.time() in  let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (Grille.eval_acc_gr g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
                else
                  if x="aa2" then let tps_deb=Sys.time() in  let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (Grille.eval_acc2_gr g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
                  else
                    if x="as" then let tps_deb=Sys.time() in let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
                    else
                      if x="as2" then let tps_deb=Sys.time() in let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (Grille.eval_str2 g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
                      else
                        let tps_deb=Sys.time() in let (nb,(i,j))=Ia.alphabeta_count g p jo nb_jo (fun j g-> (Grille.eval_str3 g) j nb_jo p) profond in let tps_fin=Sys.time() in coup js (Grille.jouer i j jo g) (((tps_fin-.tps_deb),nb,k)::res)
   
  with
  |Grille.Grille_pleine ->(Printf.printf "Nul\n"); res
  |Score.Fin_partie s->(Printf.printf "Victoire joueur %s!\n" s);res
  |Grille.Coup_impossible->(Printf.printf "Coup invalide \n";coup k g res)
  in 
  let l1=List.rev (coup 0 game []) and l2= List.rev (coup 1 game [] )in
  affichage_comparaison l1 l2

  let statistiques n m p nb_jo profond t s=
  let res=Array.make_matrix nb_jo (nb_jo+1) 0 in
  for z=1 to s do
  let game,jc=generer_grille n m nb_jo t in
  let rec coup k diff g =
  try
    Grille.test_victoire_gr g p;
    let x=Sys.argv.(((k+diff) mod nb_jo)+nb_arguments)and jo=string_of_int((k) mod nb_jo) and js= ((k+1) mod nb_jo) in
    if x="mn" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_naif_gr g) j nb_jo p) profond in coup js diff (Grille.jouer i j jo g) 
    else
      if x="ma" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_acc_gr g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
      else
        if x="ma2" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_acc2_gr g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
        else 
          if x="ms" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
          else
            if x="ms2" then let (i,j)=Ia.minimax g p jo nb_jo (fun j g-> (Grille.eval_str2 g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
            else
              if x="an" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_naif_gr g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
              else
                if x="aa" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_acc_gr g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
                else
                  if x="aa2" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_acc2_gr g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
                  else
                    if x="as" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_str g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
                    else
                      if x="as2" then let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_str2 g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
                      else
                          let (i,j)=Ia.alphabeta g p jo nb_jo (fun j g-> (Grille.eval_str3 g) j nb_jo p) profond in   coup js diff (Grille.jouer i j jo g) 
   
  with
  |Grille.Grille_pleine ->res.(diff).(nb_jo)<-res.(diff).(nb_jo)+ 1
  |Score.Fin_partie s->let i=int_of_string(s) in res.(diff).(i)<-res.(diff).(i)+ 1
  |Grille.Coup_impossible->coup k diff g
  in
  coup 0 0 game;
  coup 0 1 game;
done;
  Array.iteri (fun i x->Array.iteri (fun j y->if j=nb_jo then Printf.printf "Nombre de match nul lorsque %d commence : %d \n" i y else Printf.printf "Nombre de victoire de %d lorsque %d commence : %d \n" j i y ) x) res;;

let () = statistiques (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2))) (int_of_string(Sys.argv.(3))) ((Array.length Sys.argv)-nb_arguments)  (int_of_string(Sys.argv.(4))) (float_of_string(Sys.argv.(5))) (int_of_string(Sys.argv.(6)))
(*statistiques (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2))) (int_of_string(Sys.argv.(3))) ((Array.length Sys.argv)-nb_arguments)  (int_of_string(Sys.argv.(4))) (float_of_string(Sys.argv.(5))) (int_of_string(Sys.argv.(6)))*)
(*comparaison  (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2))) (int_of_string(Sys.argv.(3))) ((Array.length Sys.argv)-nb_arguments)  (int_of_string(Sys.argv.(4))) (float_of_string(Sys.argv.(5)))*)
(*jouer_partie (int_of_string(Sys.argv.(1))) (int_of_string(Sys.argv.(2))) (int_of_string(Sys.argv.(3)))  ((Array.length Sys.argv)-nb_arguments)  (int_of_string(Sys.argv.(4)))*)