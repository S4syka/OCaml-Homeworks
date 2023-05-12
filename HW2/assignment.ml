let f1 acc (a, b) = acc @ [(b, a)];;
let f2 acc element = element :: List.rev acc
let f3 acc_fun (k, v) = fun ki -> if ki = k then v else acc_fun ki

let rec map_tr ?(acc = []) f = function
| [] -> List.rev acc
| x :: xs -> map_tr ~acc:(f (x) :: acc) f xs;;

let rec replicate_tr ?(acc = []) n x =
  if n < 1 then acc else replicate_tr ~acc:(acc @ [x]) (n-1) x;;

type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)
type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)

let rec map_over_custom_llist f lst = match lst() with
| NilC -> fun () -> NilC
| ConsC (x, xs) -> fun () -> ConsC(f x, map_over_custom_llist f xs);;

let rec map_over_ocaml_llist f lst = match (Lazy.force lst) with
| NilO -> lazy NilO
| ConsO (x, xs) -> lazy (ConsO (f x, map_over_ocaml_llist f xs));;

let rec merge_custom_llists l1 l2 = match (l1(), l2()) with
| (NilC, NilC) -> fun () -> NilC
| (NilC, ConsC(y, ys)) -> l2
| (ConsC(x, xs), NilC) -> l1
| (ConsC(x, xs), ConsC(y, ys)) -> 
  if x > y then fun() -> ConsC(y, merge_custom_llists l1 ys)
  else fun() -> ConsC(x, merge_custom_llists xs l2)

let rec merge_ocaml_llists l1 l2 = match (Lazy.force l1, Lazy.force l2) with
| (NilO, NilO) -> lazy NilO
| (NilO, ConsO(y, ys)) -> l2
| (ConsO(x, xs), NilO) -> l1
| (ConsO(x, xs), ConsO(y, ys)) -> 
  if x > y then lazy (ConsO(y, merge_ocaml_llists l1 ys))
  else lazy (ConsO(x, merge_ocaml_llists xs l2))

let rec drop_dupl_custom_llist list = match list() with
| NilC -> fun () -> NilC
| ConsC (x, xs) -> match xs() with
  | NilC -> (fun () -> ConsC(x, fun () -> NilC))
  | ConsC (y, ys) -> if y = x then fun () -> ConsC (y, drop_dupl_custom_llist ys)
                      else fun() -> ConsC(x, drop_dupl_custom_llist xs)

let rec drop_dupl_ocaml_llist list = match Lazy.force list with
| NilO -> lazy NilO
| ConsO (x, xs) -> match (Lazy.force xs) with
  | NilO -> lazy (ConsO (x, lazy NilO))
  | ConsO (y, ys) -> if y = x then lazy (ConsO (y, drop_dupl_ocaml_llist ys))
                      else lazy (ConsO(x, drop_dupl_ocaml_llist xs))

let rec hamming_ocaml_llist list = match Lazy.force list with
| NilO -> lazy NilO
| ConsO (x, xs) -> let a = lazy (ConsO (5 * x, lazy NilO))
                    in let b = lazy (ConsO (3 * x, a))
                    in let c = lazy (ConsO (2 * x, b))
                    in lazy (ConsO (x, hamming_ocaml_llist (drop_dupl_ocaml_llist (merge_ocaml_llists c xs))))

let rec hamming_custom_llist list = match list() with
| NilC -> fun() -> NilC
| ConsC (x, xs) -> let a = fun() -> ConsC (5 * x, fun () -> NilC)
                    in let b = fun() -> (ConsC (3 * x, a))
                    in let c = fun() -> (ConsC (2 * x, b))
                    in fun() -> ConsC (x, hamming_custom_llist (drop_dupl_custom_llist (merge_custom_llists c xs)))

let hamming_custom = hamming_custom_llist (fun () -> ConsC(1, fun() -> NilC))
let hamming_ocaml = hamming_ocaml_llist (lazy (ConsO (1, lazy NilO)))




(* -----------------------------------------------------------------------------
 *  TESTING: Some simple tests for functions f1, f2 and f3. 
 *  If testing_fs () does not succeed, please check the line numbers in the 
 *  returend list to see which test failed, and then check again your solution.
 *  The tests should not be modified.
 * -----------------------------------------------------------------------------
 *)                                    

 let testing_fs () =
  let l =
    [
      __LINE_OF__ ((List.fold_left f1 [] [(1,2); (3,4); (5,6)]) =
                     [(2,1); (4,3); (6,5)]);
      __LINE_OF__ ((List.fold_left f2 [] ['a';'b';'c';'d';'e';'f';'g']) =
                     ['g';'e';'c';'a';'b';'d';'f']);
      __LINE_OF__ (let g = List.fold_left f3 (fun _ -> 0)
                             [('a',3); ('z', -9); ('d', 18)] in
                   (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9))
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The f1, f2, f3 test succeeds.\n"; [])
  else (Printf.printf "The f1, f2, f3 test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)




(* -----------------------------------------------------------------------------
*  TESTING: Simple tests for map_tr and replicate_tr. 
*  If test_tr_llist () says that the test did not succeed
*  please check the returned line numbers to see which tests failed, 
*  and then check again your solution.
*  The tests should not be modified.
* -----------------------------------------------------------------------------
*)     

let test_tr_llist () =
 let l =
   [
     __LINE_OF__ (map_tr succ [1;2;3] = [2;3;4]);
     __LINE_OF__ (map_tr (fun x -> x^x) ["a";"b";"c"] = ["aa";"bb";"cc"]);
     __LINE_OF__ (replicate_tr 5 "a" = ["a";"a";"a";"a";"a"]);
     __LINE_OF__ (replicate_tr (-3) "a" = [])
   ] in
 let result = List.fold_left (&&) true (List.map snd l) in
 if result then (Printf.printf "The tests for map and replicate succeed.\n"; [])
 else (Printf.printf "The test for tests for map and replicate fail.\n Check the corresponding line numbers in the list below.\n";
       (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* -----------------------------------------------------------------------------
*  TESTING: Helper functions used for testing lazy lists
* -----------------------------------------------------------------------------
*)    

let rec from_to_custom from to_ step =
     if from <= to_
     then fun () -> ConsC (from, from_to_custom (from + step) to_ step)
     else fun () -> NilC

let rec print_custom_llist n c_list =
 if n != 0
 then match c_list () with
      | NilC -> print_string "Nil\n"
      | ConsC (h, t) ->
         Printf.printf "%d, " h;
         print_custom_llist (n-1) t
 else print_string "...\n"

let rec custom_llist_to_string n c_list =
 if n != 0
 then match c_list () with
   | NilC -> "Nil"
   | ConsC (h, t) ->
      string_of_int h ^ ", " ^
        custom_llist_to_string (n-1) t
 else "..."

let rec from_to_ocaml from to_ step =
     if from <= to_
     then lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
     else lazy NilO

let rec print_ocaml_llist n o_list =
 if n != 0
 then match Lazy.force o_list with
   | NilO -> print_string "Nil\n"
   | ConsO (h, t) ->
      Printf.printf "%d, " h;
      print_ocaml_llist (n-1) t
 else print_string "...\n"

let rec ocaml_llist_to_string n o_list =
 if n != 0
 then match Lazy.force o_list with
   | NilO -> "Nil"
   | ConsO (h, t) ->
      string_of_int h ^ ", " ^
        ocaml_llist_to_string (n-1) t
 else "..."

(* -----------------------------------------------------------------------------
*  TESTING: Simple tests for map_over_custom_llist and map_over_ocaml_llist. 
*  If test_map_llist () says that the test did not succeed
*  please check the returned line numbers to see which tests failed, 
*  and then check again your solution.
*  The tests should not be modified.
* -----------------------------------------------------------------------------
*)     

let test_map_llist () =
 let l =
   [
     __LINE_OF__ (custom_llist_to_string 10
       (map_over_custom_llist (fun x -> x+1) (from_to_custom 0 5 1)) =
                    "1, 2, 3, 4, 5, 6, Nil");
     __LINE_OF__ (custom_llist_to_string 10
       (map_over_custom_llist (fun x -> x+1) (from_to_custom 6 5 1)) =
                    "Nil");
      __LINE_OF__ (ocaml_llist_to_string 10
       (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 0 5 1)) =
                     "1, 2, 3, 4, 5, 6, Nil");
       __LINE_OF__ (ocaml_llist_to_string 10
       (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 6 5 1)) =
                      "Nil")
   ] in
 let result = List.fold_left (&&) true (List.map snd l) in
 if result then (Printf.printf "The test for mapping over lazy lists succeeds.\n"; [])
 else (Printf.printf "The test for mapping over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
       (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* -----------------------------------------------------------------------------
*  TESTING: Simple tests for merge_custom_llists and merge_ocaml_llists. 
*  If test_merge_llists () says that the test did not succeed
*  please check the returned line numbers to see which tests failed, 
*  and then check again your solution.
*  The tests should not be modified.
* -----------------------------------------------------------------------------
*)     

let test_merge_llists () =
 let l =
   [
     __LINE_OF__ (custom_llist_to_string 13
       (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 0 5 1)) =
                    "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
     __LINE_OF__ (custom_llist_to_string 13
                    (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 6 5 1)) =
                    "0, 1, 2, 3, 4, 5, Nil");
     __LINE_OF__ (ocaml_llist_to_string 13
       (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 0 5 1)) =
                    "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
     __LINE_OF__ (ocaml_llist_to_string 13
                    (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 6 5 1)) =
                    "0, 1, 2, 3, 4, 5, Nil")
   ] in
 let result = List.fold_left (&&) true (List.map snd l) in
 if result then (Printf.printf "The test for merging over lazy lists succeeds.\n"; [])
 else (Printf.printf "The test for merging over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
       (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* -----------------------------------------------------------------------------
*  TESTING: Simple tests for drop_dupl_custom_llist and drop_dupl_ocaml_llist. 
*  If test_drop_dupl_llists () says that the test did not succeed
*  please check the returned line numbers to see which tests failed, 
*  and then check again your solution.
*  The tests should not be modified.
* -----------------------------------------------------------------------------
*)     

let test_drop_dupl_llists () =
 let l =
   [
     __LINE_OF__ (custom_llist_to_string 13
                    (drop_dupl_custom_llist
                       (merge_custom_llists (from_to_custom 0 5 1)
                          (from_to_custom 0 5 2))) =
                    "0, 1, 2, 3, 4, 5, Nil");
     __LINE_OF__ (custom_llist_to_string 13
                    (drop_dupl_custom_llist
                       (merge_custom_llists (from_to_custom 0 5 1)
                          (from_to_custom 6 5 1))) =
                    "0, 1, 2, 3, 4, 5, Nil");
     __LINE_OF__ (ocaml_llist_to_string 13
                    (drop_dupl_ocaml_llist
                       (merge_ocaml_llists (from_to_ocaml 0 5 1)
                          (from_to_ocaml 0 5 1))) =
                    "0, 1, 2, 3, 4, 5, Nil");
     __LINE_OF__ (ocaml_llist_to_string 13
                    (drop_dupl_ocaml_llist
                       (merge_ocaml_llists (from_to_ocaml 0 5 1)
                          (from_to_ocaml 6 5 1))) =
                    "0, 1, 2, 3, 4, 5, Nil")
   ] in
 let result = List.fold_left (&&) true (List.map snd l) in
 if result then (Printf.printf "The test for dropping duplicates from  lazy lists succeeds.\n"; [])
 else (Printf.printf "The test for dropping duplicates from lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
       (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* -----------------------------------------------------------------------------
*  TESTING: Simple tests for hamming_custom and hamming_ocaml. 
*  If test_hamming_llists () says that the test did not succeed
*  please check the returned line numbers to see which tests failed, 
*  and then check again your solution.
*  The tests should not be modified.
* -----------------------------------------------------------------------------
*)     

let test_hamming_llists () =
 let l =
   [
     __LINE_OF__ (custom_llist_to_string 14 hamming_custom =
                    "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
     __LINE_OF__ (custom_llist_to_string 20 hamming_custom = 
                    "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...");
     __LINE_OF__ (ocaml_llist_to_string 14 hamming_ocaml =
                    "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
     __LINE_OF__ (ocaml_llist_to_string 20 hamming_ocaml = 
                    "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...")
   ] in
 let result = List.fold_left (&&) true (List.map snd l) in
 if result then (Printf.printf "The test for Hamming lists succeeds.\n"; [])
 else (Printf.printf "The test for hamming lists fails.\n Check the corresponding line numbers in the list below.\n";
       (List.filter (fun (x,y) -> y=false) l) |> List.map fst)