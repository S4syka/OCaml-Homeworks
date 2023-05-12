(*Subtask_1*)
let equal_second_components (_, x) (_, y) = compare x y;;

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)

let rec member c t l = match l with
  |[] -> false
  |head :: tail -> c t head = 0 || member c t tail;;
;;

(*Subtask_2*)
let rec insert_by_second_value value list = match list with
  | [] -> [value]
  | head :: tail -> if snd value >= snd head then value :: list else head :: insert_by_second_value value tail

let rec insertion_sort_by_second_value = function
  | [] -> []
  | head :: tail -> insert_by_second_value head (insertion_sort_by_second_value tail)

let rec add_occurence value list2 = match list2 with
  |[] -> [(value, 1)]
  |head :: tail -> 
    if fst (head) = value then
      (value, snd (head) + 1) :: tail
    else head :: add_occurence value tail;;
 
let rec count_occurrences_unsorted list1 = match list1 with
  |[] -> []
  |head :: tail -> add_occurence head (count_occurrences_unsorted tail);;
  
let count_occurrences list1 =
  insertion_sort_by_second_value (count_occurrences_unsorted (List.rev list1));;

(*Subtask_3*)
let drop_last list = match List.rev list with
  | [] -> raise (Failure "Empty list has no last element")
  | head :: tail -> List.rev tail;;

(*Subtask_4*)  
let drop_last_opt list = match List.rev list with
| [] -> None
| head :: tail -> Some (List.rev tail);;

(*Subtask_5*)
let rec zip_with f list1 list2 = 
  let comb = (list1, list2) in match comb with
  | ([], _) -> []
  | (_, []) -> []
  | (h1::t1, h2::t2) -> f h1 h2 :: zip_with f t1 t2;;

(*Subtask_6*)
let unzip list1 =
  List.fold_left (fun acc value -> (fst value :: fst acc , snd value :: snd acc)) ([],[]) (List.rev list1);;

(*Subtask_7*)

(*
  unzip [('a',1); ('b',2)] = (['a';'b'], [1;2])

  Unzip function takes list [('a',1); ('b',2)] and folds this list (Operations should be applied to the reversed list, unless output will be in
  reversed order.) with ([],[]) as an accumulator and (fun acc value -> (fst value :: fst acc , snd value :: snd acc) as a folding function

  At first ([],[]) will be empty.
  After that folding function will append ('b', 2) to ([], []) so that ([], []) becomes (['b'], [2]);
  After that folding function will append ('a', 1) to  (['b'], [2]) so that we will get (['a', 'b'], [1, 2])
  and the unzip funcion will return the accumulator - (['a', 'b'], [1, 2])
*)

(*Subtask-8*)

type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha;;

(*A*)

let rec add_team_info name gf1 ga1 teams_info = match teams_info with
  | [] -> if gf1 > ga1 then [(name, 1, 1, 0, 0, gf1, ga1, 3)]
    else if gf1 == ga1 then [(name, 1, 0, 1, 0, gf1, ga1, 1)]
    else [(name, 1, 0, 0, 1, gf1, ga1, 0)]
  | (t, g, w, d, l, gf, ga, p) :: tail -> if(t != name) then (t, g, w, d, l, gf, ga, p) :: add_team_info name gf1 ga1 tail
    else if gf1 > ga1  then (name, g + 1, w + 1, d, l, gf + gf1, ga + ga1, p + 3) :: tail
    else if gf1 == ga1 then (name, g + 1, w, d + 1, l, gf + gf1, ga + ga1, p + 1) :: tail
    else (name, g + 1, w, d, l + 1, gf + gf1, ga + ga1, p) :: tail;;

let rec countries_info match_list = match match_list with
  | [] -> []
  | (team1, game1, team2, game2) :: tail -> add_team_info team1 (List.length game1) (List.length game2) (add_team_info team2 (List.length game2) (List.length game1) (countries_info tail));;

let rec insert_sort_countries_info (t, g, w, d, l, gf, ga, p) sorted_list = match sorted_list with
  | [] -> [(t, g, w, d, l, gf, ga, p)]
  | (st, sg, sw, sd, sl, sgf, sga, sp) :: tail -> if p > sp then (t, g, w, d, l, gf, ga, p) :: sorted_list
    else if p == sp && (gf - ga) > (sgf - sga) then (t, g, w, d, l, gf, ga, p) :: sorted_list
    else if p == sp && (gf - ga) == (sgf - sga) && gf > sgf then (t, g, w, d, l, gf, ga, p) :: sorted_list
    else (st, sg, sw, sd, sl, sgf, sga, sp) :: insert_sort_countries_info (t, g, w, d, l, gf, ga, p) (tail);;

let rec sort_countries_info info = match info with
  | [] -> []
  | h :: tail -> insert_sort_countries_info h (sort_countries_info tail);;

(*B*)

let rec insert_sort_scores_info (name, team, score) sorted_list = match sorted_list with 
  | [] -> [(name, team, score)]
  | (sname, steam, sscore) :: tail -> if score > sscore then (name, team ,score) :: sorted_list
    else if score == sscore && name < sname then (name, team, score) :: sorted_list
    else (sname, steam, sscore) :: (insert_sort_scores_info (name, team, score) tail);; 

let rec sort_scores_info info = match info with
| [] -> []
| h :: tail -> insert_sort_scores_info h (sort_scores_info tail)

let thrd (a, b, c) = c;;

let rec add_score name team scores = if name = "OG" then scores else match scores with 
  | [] -> [(name, team, 1)]
  | (n, t, scnt) :: tail -> if ((n = name) && (t = team))
    then (name, team, scnt + 1) :: tail
    else (n, t, scnt) :: (add_score name team tail);; 

let rec add_scores team goals scores = match goals with
  |[] -> scores
  |h :: tail -> add_score h team (add_scores team tail scores);;

let rec scores_info match_list = match match_list with
  |[] -> []
  |(team1, game1, team2, game2) :: tail -> (add_scores team1 game1 (add_scores team2 game2 (scores_info tail)));;

let table_and_scorers match_list = 
  (sort_countries_info (countries_info match_list), sort_scores_info (scores_info match_list));;


(*Tests*)
let testing_member () =
  let l =
    [
      __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
      __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
      __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
      __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
      __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The member test succeeds.\n"; [])
  else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;

let testing_count_occurrences () =
  let l =
    [
      __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
      __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
      __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
      __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
  else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;

let testing_drop_last () =
  let l =
    [
      __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
      __LINE_OF__ ((drop_last [1]) = []);
      __LINE_OF__ ((try Some (drop_last []) with (Failure _) -> None) = None) (* If this line is reported during testing, you have an rrror in raising Failure *)
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The drop_last test succeeds.\n"; [])
  else (Printf.printf "The drop_last test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;

let testing_drop_last_opt () =
  let l =
    [
      __LINE_OF__ ((drop_last_opt []) = None);
      __LINE_OF__ ((drop_last_opt [1]) = Some []);
      __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2])
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The drop_last_opt test succeeds.\n"; [])
  else (Printf.printf "The drop_last_opt test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;

let testing_zip_with () =
   let l =
     [
       __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
       __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8]) = [[1; 5]; [2; 6]; [3; 7]]);
       __LINE_OF__ ((zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
       __LINE_OF__ ((zip_with (+) [1;2;3] [5;6]) =[6; 8]);
       __LINE_OF__ ((zip_with (^) ["aa";"bb";"cc"] ["1";"2"]) = ["aa1"; "bb2"]);

     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The zip_with test succeeds.\n"; [])
   else (Printf.printf "The zip_with test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;

let testing_unzip () =
   let l =
     [
       __LINE_OF__ ((unzip [('a',1); ('b',2)]) = (['a';'b'], [1;2]));
       __LINE_OF__ ((unzip []) = ([], []));
       __LINE_OF__ ((unzip [('a',1)]) = (['a'], [1]));

     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The unzip test succeeds.\n"; [])
   else (Printf.printf "The unzip test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ];;
let wc22_H = 
  [(Uru, [], Kor, []);
   (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
   (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
   (Por, ["Fernandes"; "Fernandes"], Uru, []);
   (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
   (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
  ];;

let testing_table_and_scorers () =
  let l =
    [
      __LINE_OF__ (table_and_scorers wc22_H =
                     ([(Por, 3, 2, 0, 1, 6, 4, 6);
                       (Kor, 3, 1, 1, 1, 4, 4, 4);
                       (Uru, 3, 1, 1, 1, 2, 2, 4);
                       (Gha, 3, 1, 0, 2, 5, 7, 3)],
                      [("Cho Gue-sung", Kor, 2);
                       ("De Arrascaeta", Uru, 2);
                       ("Fernandes", Por, 2);
                       ("Kudus", Gha, 2);
                       ("Ayew", Gha, 1);
                       ("Bukari", Gha, 1);
                       ("Felix", Por, 1);
                       ("Horta", Por, 1);
                       ("Hwang Hee-chan", Kor, 1);
                       ("Kim Young-gwon", Kor, 1);
                       ("Leao", Por, 1);
                       ("Ronaldo", Por, 1);
                       ("Salisu", Gha, 1)]));
      __LINE_OF__ (table_and_scorers wc22_C =
                     ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                       (Pol, 3, 1, 1, 1, 2, 2, 4);
                       (Mex, 3, 1, 1, 1, 2, 3, 4);
                       (Sau, 3, 1, 0, 2, 3, 5, 3)],
                      [("Al-Dawsari", Sau, 2);
                       ("Messi", Arg, 2);
                       ("Al-Shehri", Sau, 1);
                       ("Alvarez", Arg, 1);
                       ("Chavez", Mex, 1);
                       ("Fernandez", Arg, 1);
                       ("Lewandowski", Pol, 1);
                       ("Mac Allister", Arg, 1);
                       ("Martin", Mex, 1);
                       ("Zielinski", Pol, 1)]))
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
  else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;