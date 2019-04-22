open OUnit2
open Part1

(* Below are some examples of unit tests. These are not at all complete!!! Add your own unit tests. *)
  
type my_u = A | B | C

let string_of_my_u = function
  | A -> "a"
  | B -> "b"
  | C -> "c"
  
(** Part 1 tests *)

let m1 =
  Multiset.(add empty C)
  
let m2 =
  Multiset.(add (add empty C) A)

let m3 =
  Multiset.(add (add (add empty C) A) A)


let part1_count_tests =
  (* multiset, base element, expected result for count *)
  [Multiset.empty, A, 0;
   Multiset.empty, B, 0;
   Multiset.empty, C, 0;
   m1, A, 0;
   m1, B, 0;
   m1, C, 1;
   m2, A, 1;
   m2, B, 0;
   m2, C, 1;
   m3, A, 2;
   m3, C, 1;
   m3, B, 0]

let string_of_multiset m = Multiset.to_string string_of_my_u m
    
let part1_count_suite =
  List.map (fun (m, x, expected_result) ->
    let name = Printf.sprintf "count %s %s" (string_of_multiset m) (string_of_my_u x) in
    name >::
    fun tc ->
      assert_equal ~printer:string_of_int expected_result (Multiset.count m x))
    part1_count_tests


let part1_suite =
  "Part 1 suite" >:::
  part1_count_suite

let _ = run_test_tt_main part1_suite

(** Part 2 tests *)

module MyMultiset = Part2.Make(struct
  type t = my_u
  let compare x y = match x, y with
  | A, (B | C)
  | B, C -> -1
  | C, (A | B)
  | B, A -> 1
  | _ -> assert (x = y); 0
end)

let m1 =
  MyMultiset.(add empty C)
  
let m2 =
  MyMultiset.(add (add empty C) A)

let m3 =
  MyMultiset.(add (add (add empty C) A) A)


let part2_count_tests =
  (* multiset, base element, expected result for count *)
  [MyMultiset.empty, A, 0;
   MyMultiset.empty, B, 0;
   MyMultiset.empty, C, 0;
   m1, A, 0;
   m1, B, 0;
   m1, C, 1;
   m2, A, 1;
   m2, B, 0;
   m2, C, 1;
   m3, A, 2;
   m3, C, 1;
   m3, B, 0]

let part2_count_suite =
  List.map (fun (m, x, expected_result) ->
    let name = "count" in
    name >::
    fun tc ->
      assert_equal expected_result (MyMultiset.count m x))
    part2_count_tests


let part2_suite =
  "Part 2 suite" >:::
  part2_count_suite

    
let _ = run_test_tt_main part2_suite


(** Part 3 tests ... *)

open Part3

module MyOMultiset = Part3.Make(struct
  type t = int
  let compare = compare
end)

let part3_compare_tests =
  (* m1, m2, test for result of compare *)
  [[1; 1; 1; 2; 2], [1; 1; 3], (fun cmp -> cmp < 0);
   [1; 1; 3], [1; 1; 1; 2; 2], (fun cmp -> cmp > 0);
   [1; 1; 2; 2], [1; 1; 1; 2; 2], (fun cmp -> cmp < 0);
   [1; 1; 1; 2; 2], [1; 1; 2; 2], (fun cmp -> cmp > 0);
   [], [], (fun cmp -> cmp = 0);
   [1], [1], (fun cmp -> cmp = 0);
   [1; 1], [1; 1], (fun cmp -> cmp = 0);
   [1; 1; 3], [1; 3; 1], (fun cmp -> cmp = 0);
   [1; 1; 3; 2; 2], [2; 1; 3; 1; 2], (fun cmp -> cmp = 0);
   [], [1], (fun cmp -> cmp < 0);
   [1], [], (fun cmp -> cmp > 0);
   [], [1; 1], (fun cmp -> cmp < 0);
   [1; 1], [], (fun cmp -> cmp > 0);
   [], [1; 2; 1], (fun cmp -> cmp < 0);
   [1; 2; 1], [], (fun cmp -> cmp > 0);
 ]

let part3_count_suite =
  List.map (fun (m1, m2, p) ->
    let m1 = MyOMultiset.of_list m1 in
    let m2 = MyOMultiset.of_list m2 in
    let name =
      Printf.sprintf "compare %s %s"
        (MyOMultiset.to_string string_of_int m1)
        (MyOMultiset.to_string string_of_int m2) in
    name >::
    fun tc ->
      assert (p @@ MyOMultiset.compare m1 m2))
    part3_compare_tests


let part3_suite =
  "Part 2 suite" >:::
  part3_count_suite

    
let _ = run_test_tt_main part3_suite
