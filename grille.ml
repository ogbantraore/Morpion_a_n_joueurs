let compare_t= fun t1 t2-> let c= compare (fst t1) (fst t2) in if c=0 then compare (snd t1) (snd t2) else c

module EC=Set.Make(struct type t=int*int let compare=compare_t end)

exception Grille_pleine
exception Coup_impossible
type t ={grille:string array array;cases_vides: EC.t}

let est_pleine g=EC.is_empty g.cases_vides

let creer_grille n m =
  let g=Array.make_matrix n m "_" and cv=ref EC.empty in
    Array.iteri (fun i x -> Array.iteri (fun j _->cv:=EC.add (i,j) !cv) x) g;
    {grille=g;cases_vides=(!cv)}

let coups g=EC.elements g.cases_vides

let jouer i j p g= if (EC.mem (i,j) g.cases_vides)
   then let gr=Array.(map copy) g.grille in (gr.(i).(j)<-p; {grille=gr;cases_vides=(EC.remove (i,j) g.cases_vides)})
 else if (est_pleine g) then raise Grille_pleine else raise Coup_impossible

 let afficher_grille g=
  let gr=g.grille in
    Array.iter (fun x->Array.iter (fun y-> print_string(y)) x; print_string("\n")) gr;print_string("\n")

let eval_naif_gr g=let gr=g.grille in Score.eval_naif gr
let eval_acc_gr g=let gr=g.grille in Score.eval_acc gr

let eval_acc2_gr g =let gr=g.grille in Score.eval_acc2 gr
let generer_grille n m nb_jo t=
let g=creer_grille n m in
let nb_coups=int_of_float (float_of_int(n*m)*.t) in
let rec remplir = fun k x g->
  if k=0 then g,x
  else
    let lc=coups g in
    let len=List.length lc in
    let (i,j)=List.nth lc (Random.int len) in
    remplir (k-1) (string_of_int( ((int_of_string x)+1) mod nb_jo)) (jouer i j x g) 
  in
  remplir nb_coups "0" g


let test_victoire_gr g p=let gr=g.grille in Score.test_victoire gr p; if (est_pleine g) then raise Grille_pleine else ()

let eval_str g=let gr=g.grille in Score.eval_str gr

let eval_str2 g=let gr=g.grille in Score.eval_str2 gr
let eval_str3 g=let gr=g.grille in Score.eval_str3 gr