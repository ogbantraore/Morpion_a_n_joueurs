exception Fin_partie of string

let tableau_points=[|0;0;1;3;12;70;500;2000;7000;10000;20000;100000|]

let score_acc= fun jo p x y acc g->
                  let (score,long, prec,mem)=acc in
                  if g.(x).(y)=prec then 
                  (if prec="_" then 
                    (score,(long),prec,mem)
                    else
                      if (long+1) >= p  then
                        (raise (Fin_partie prec)) 
                      else 
                        (score,(long + 1),prec,mem))
                  else
                    (if g.(x).(y)="_" then 
                      ((score,long,"_",prec))
                    else 
                      (if prec="_" && g.(x).(y) = mem then 
                        if (long+1) >= p then 
                          (raise (Fin_partie mem)) 
                        else (score,(long + 1),mem,mem) 
                      else 
                        (if prec=jo then 
                          (score+tableau_points.(long),1,g.(x).(y),g.(x).(y)) 
                        else
                          ((score-tableau_points.(long),1,g.(x).(y),g.(x).(y))))))
let score_acc2= fun jo p x y acc g->
                        let (score,long, prec,mem)=acc in
                        if g.(x).(y)=prec then 
                          (if prec="_"  then 
                              (score,1,"_","_") 
                          else  
                            if (long+1>p) ||((long+1)=p && mem<>prec )then
                              (raise (Fin_partie prec)) 
                            else 
                              (score,(long + 1),prec,mem))
                        else 
                          (if g.(x).(y)="_" then
                            (if prec=jo then 
                              (score+tableau_points.(long+1),1,g.(x).(y),"_")
                            else
                              ((score-tableau_points.(long+1),1,g.(x).(y),"_")))
                          else
                            (if prec="_" then
                                (score,long+1,g.(x).(y),g.(x).(y))
                            else
                              if mem=prec then 
                                (if prec=jo then 
                                  (score+tableau_points.(long),1,g.(x).(y),"_")
                                else
                                  ((score-tableau_points.(long),1,g.(x).(y),"_")))
                                else
                              (score,1,g.(x).(y),"_")))

                            
                              
      

let fonction_alignements=fun f fb fj fs g z->
              let n=Array.length g and m=Array.length g.(0) in
                let iter_direction= fun dx dy a b ->
                  let rec iter x y acc=  if ((y<0 && dy<0)||(x<0 && dx<0) || (x>(n-1) && dx>0) || (y>(m-1) && dy>0)) then fb acc
                      else iter (x+dx) (y+dy) (f x y acc g) in
                      iter a b (fs g a b) in
                  let rec rech_case= fun i j z->
                    if i=0 then (fj (fj (fj (iter_direction 1 0 0 j ) (fj (iter_direction 1 1 0 j  )  (iter_direction 1 (-1) 0 j ))) (iter_direction 0 1 i 0 )) (rech_case (i+1) (j+1) z)) 
                    else
                    if (i=(n) && j=(m)) then z else (if (i=n) then
                      (fj ( fj (iter_direction 1 0 0 j) (fj (iter_direction 1 1 0 j)  (iter_direction 1 (-1) 0 j ))) (rech_case (n) (j+1) z)) 
                    else (if (j=(m)) then (fj (fj (iter_direction 0 1 i 0 ) (fj (iter_direction 1 1 i 0  )  (iter_direction 1 (-1) i (m-1) ))) (rech_case (i+1) (m) z))
                    else (fj (fj (fj (iter_direction 1 0 0 j ) (fj (iter_direction 1 1 0 j  )  (iter_direction 1 (-1) 0 j ))) (fj (iter_direction 0 1 i 0 ) (fj (iter_direction 1 1 i 0  )  (iter_direction 1 (-1) i (m-1) )))) (rech_case (i+1) (j+1) z)););); 
                  in rech_case 0 0 z
  
let test_victoire g p=fonction_alignements (fun x y a g-> let (long,prec)=a in if (g.(x).(y)=prec)&&((long+1)>=p) then (if prec="_" then (0,prec) else (raise (Fin_partie prec))) else if (g.(x).(y)=prec)&& (prec<>"_") then ((long+1),prec) else (1,g.(x).(y))) (fun (_,_)->()) ( fun _ _ ->()) (fun g a b->(0,g.(a).(b))) g ()
            
let eval_naif gr jo nb_jo p = try (test_victoire gr p);0 with
|Fin_partie v ->if v=jo then max_int else (~-max_int)
let eval_acc gr jo nb_jo p =  try fonction_alignements (score_acc jo p) (fun x->let (a,l,j,_)=x in if j=jo then a+tableau_points.(l) else (if j<>"_" then (a-tableau_points.(l)) else a)) (+) (fun g x y->(0,0,g.(x).(y),g.(x).(y))) gr 0  with
|Fin_partie v ->if v=jo then max_int else (~-max_int)
let eval_acc2 gr jo nb_jo p =  try fonction_alignements (score_acc2 jo p) (fun x->let (a,l,j,mem)=x in  if j=mem then (if j=jo then if (l>p || (l=p && mem<>jo)) then raise (Fin_partie j) else a+tableau_points.(l) else (if j<>"_" then if (l>p || (l=p && mem<>j)) then raise (Fin_partie j) else (a-tableau_points.(l)) else if mem=jo then a+tableau_points.(l) else a-tableau_points.(l))) else a) (+) (fun g x y->(0,0,g.(x).(y),"_")) gr 0  with
|Fin_partie v ->if v=jo then max_int else (~-max_int)

let eval_str g j nb_jo p =
  let jo=int_of_string j in
  try
  fonction_alignements
    (fun x y acc gr -> String.concat "" [gr.(x).(y); acc])
    (fun acc ->
      let rec split_aux ls k =
        if k = nb_jo then ls
        else (
          if k = jo then split_aux ls (k + 1)
          else (
            List.fold_left
              (fun m x -> (String.split_on_char (char_of_int (k + 48)) x) @ m)
              []
              (split_aux ls (k + 1))
          )
        )
      in
      let resultat = split_aux [acc] 0 in
      List.fold_left
        (fun m x ->
          max m
            (if String.contains x '_' && String.contains x (char_of_int (48 + jo)) then
               (let sc=String.length x in if sc>=p then raise (Fin_partie j) else sc)
             else
               0)
        )
        0
        resultat
    )
  (max) (fun x y g -> "") g 0 
   with
   Fin_partie x->max_int

let eval_str2 g j nb_jo p =
    let jo = int_of_string j in
    try
    let (a,b) = fonction_alignements
    (fun x y acc gr -> String.concat "" [gr.(x).(y); acc])
    (fun acc ->
    let rec split_aux ls k =
      if k = nb_jo then ls
      else (
        if k = jo then split_aux ls (k + 1)
      else (
      List.fold_left
      (fun m x -> (String.split_on_char (char_of_int (k + 48)) x) @ m)
      []
      (split_aux ls (k + 1))
      )
    )
    in
    let resultat = split_aux [acc] 0 in
    List.fold_left
    (fun (att, def) x ->
    let score = if String.contains x '_' then
    let sc = String.length x in
      if sc >= p then raise (Fin_partie j)
      else if String.contains x (char_of_int (48 + jo)) then tableau_points.(sc)
      else if String.contains x (char_of_int (48 + (1 - jo))) then -tableau_points.(sc)
      else 0
    else 0
    in
    (att + (max 0 score), def + (max 0 (-score)))
    )
    (0, 0) resultat
    )
    (fun (att1, def1) (att2, def2) ->
    (att1+att2, def1+def2)
    )
    (fun x y g -> "") g (0,0) in fst (a,b)
    with
    | Fin_partie x -> if x = j then (max_int) else ((~-max_int))
let eval_str3 g j nb_jo p =
      let jo = int_of_string j in
      try
        let (a,b)=fonction_alignements
      (fun x y acc gr -> String.concat "" [gr.(x).(y); acc])
      (fun acc ->
              let rec split_aux ls k js=
                if k = nb_jo then ls
                else (
            let suite = split_aux ls (k + 1) js in
                  if k = js then   suite
                  else (
        List.fold_left
                      (fun m x -> (String.split_on_char (char_of_int (k + 48)) x) @ m)
                      []
                      ( suite) 
                   )
                 )
              in
                  let rec resultat k res=
                  if k=nb_jo then res else resultat (k+1) ((split_aux [acc] 0 k)::res)
            in let liste=resultat 0 [] in            
            let string_search s c =
        String.fold_left (fun (best,prev,score) u ->
          if prev = 'u' then (best,u,score)(*debut de sequence*)
          else if u ='_' && prev = c then (*fin de sequence*)
            if score > best then (score,u,0)(*initialise score en fin de seq*) else (best,u,0)
          else if u='_' then (best,u,score)
          else if u = c  && prev ='_' then (*deb de la 2e nv seq *)
            (best, u , 1)
          else
            (best,u,score+1)) (0,'u', 1) s
            in
            List.fold_left (fun sc x->
              List.fold_left
                (fun (att, def) y ->
                  let (best,prev,score) = string_search y (char_of_int (48 + jo)) in
            let sc = if best> score then best else score in
                  if sc >= p then raise (Fin_partie j)
            else
              if String.contains y (char_of_int (48 + jo)) then
                  (att + (max 0 tableau_points.(sc)), def)
              else
                (att, def + (max 0 (-tableau_points.(sc))))

                )	(0, 0) x
           ) (0,0) liste)
          (fun (att1, def1) (att2, def2) ->
            (att1+att2, def1+def2)
          )
          (fun x y g -> "") g (0,0) in a-b
      with
      | Fin_partie x -> if x = j then (max_int) else ((~-max_int))
