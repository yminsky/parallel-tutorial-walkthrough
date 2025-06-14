open! Base
open! Stdio

(* ********************************************************************* *)
(* Simple fork/join parallelism *)
(* ********************************************************************* *)

let add4 par a b c d =
  let a_plus_b, c_plus_d = Parallel.fork_join2 par (fun _ -> a + b) (fun _ -> c + d) in
  a_plus_b + c_plus_d
;;

let run_with_par f =
  let module Scheduler = Parallel_scheduler_work_stealing in
  let scheduler = Scheduler.create () in
  let monitor = Parallel.Monitor.create_root () in
  let result = Scheduler.schedule scheduler ~monitor ~f in
  Scheduler.stop scheduler;
  result
;;

let%expect_test _ =
  let result = run_with_par (fun par -> add4 par 1 10 100 1000) in
  print_s [%sexp (result : int)];
  [%expect {| 1111 |}]
;;

(* ********************************************************************* *)
(* Averaging over binary trees *)
(* ********************************************************************* *)

let average tree =
  let rec total_and_count (tree : _ Tree.t) =
    match tree with
    | Leaf x -> ~total:x, ~count:1
    | Node (l, r) ->
      let ~total:total_l, ~count:count_l = total_and_count l in
      let ~total:total_r, ~count:count_r = total_and_count r in
      let total = total_l +. total_r in
      let count = count_l + count_r in
      ~total, ~count
  in
  let ~total, ~count = total_and_count tree in
  total /. Float.of_int count
;;

let test_tree : _ Tree.t = Node (Node (Leaf 3., Leaf 4.), Leaf 5.)

let%expect_test _ =
  print_s [%sexp (average test_tree : float)];
  [%expect {| 4 |}]
;;

let average_par par tree =
  let rec total_and_count par tree : total:float * count:int =
    match (tree : _ Tree.t) with
    | Leaf x -> ~total:x, ~count:1
    | Node (l, r) ->
      let (~total:total_l, ~count:count_l), (~total:total_r, ~count:count_r) =
        Parallel.fork_join2
          par
          (fun par -> total_and_count par l)
          (fun par -> total_and_count par r)
      in
      let total = total_l +. total_r in
      let count = count_l + count_r in
      ~total, ~count
  in
  let ~total, ~count = total_and_count par tree in
  total /. Float.of_int count
;;

let%expect_test _ =
  print_s [%sexp (run_with_par (fun par -> average_par par test_tree) : float)];
  [%expect {| 4 |}]
;;

let rec random_tree rng size : _ Tree.t =
  if size = 1
  then Leaf (Splittable_random.float rng ~lo:0.0 ~hi:1.0)
  else (
    let left_size = Splittable_random.int rng ~lo:1 ~hi:(size - 1) in
    let right_size = size - left_size in
    let rng' = Splittable_random.split rng in
    let left = random_tree rng left_size in
    let right = random_tree rng' right_size in
    Node (left, right))
;;

let%expect_test _ =
  let rng = Splittable_random.create (Random.State.make [| 1; 2; 3 |]) in
  let random_tree = random_tree rng 30 in
  print_s [%sexp (random_tree : float Tree.t)];
  [%expect
    {|
    (Node
     (Node
      (Node (Node (Leaf 0.65942986774926027) (Leaf 0.14518009245259245))
       (Node
        (Node
         (Node (Leaf 0.73178060004502687)
          (Node (Leaf 0.73148792973397769) (Leaf 0.10111771941178094)))
         (Node (Leaf 0.51014599083858714)
          (Node (Leaf 0.78015945924973085)
           (Node (Leaf 0.76291564458440364) (Leaf 0.16914763139792)))))
        (Leaf 0.57671788814863234)))
      (Node
       (Node (Node (Leaf 0.56336051309726454) (Leaf 0.49781793027099908))
        (Node (Node (Leaf 0.28764416405982174) (Leaf 0.36462443952692736))
         (Node (Leaf 0.29384335793261429) (Leaf 0.65736105810474765))))
       (Node
        (Node (Node (Leaf 0.81997979816951239) (Leaf 0.98712316519458854))
         (Leaf 0.13783706632919568))
        (Leaf 0.55043902091547192))))
     (Node
      (Node
       (Node (Leaf 0.81669329393820922)
        (Node (Leaf 0.068477923558238674) (Leaf 0.46636531822909433)))
       (Node (Leaf 0.83054404122300418) (Leaf 0.37621352886696546)))
      (Node
       (Node (Node (Leaf 0.478800759854746) (Leaf 0.36941883302399259))
        (Leaf 0.16153855632349845))
       (Node (Leaf 0.58384236731122763) (Leaf 0.31807143139542415)))))
    |}];
  let avg = run_with_par (fun par -> average_par par random_tree) in
  print_s [%sexp (avg : float)];
  [%expect {| 0.49326931303124849 |}]
;;

(*
   let rec random_tree_par par rng size : _ Tree.t =
  if size = 1
  then Leaf (Splittable_random.float rng ~lo:0.0 ~hi:1.0)
  else (
    let left_size = Splittable_random.int rng ~lo:1 ~hi:(size - 1) in
    let right_size = size - left_size in
    let rng' = Splittable_random.split rng in
    let left, right =
      Parallel.fork_join2
        par
        (fun par -> random_tree_par par rng left_size)
        (fun par -> random_tree_par par rng' right_size)
    in
    Node (left, right))
;;
*)
