open! Core
open! Import

let divide list =
  let rec rev_divide list ~left ~right =
    match list with
    | [] -> left, right
    | [ x ] -> x :: left, right
    | x :: y :: tl -> rev_divide tl ~left:(x :: left) ~right:(y :: right)
  in
  let left, right = rev_divide list ~left:[] ~right:[] in
  List.rev left, List.rev right
;;

let sqrts par list =
  let results = Atomic.make [] in
  let rec loop par list =
    match list with
    | [] -> ()
    | [ x ] ->
      let v = x, Float.sqrt x in
      Atomic.update results ~pure_f:(fun results -> v :: results)
    | _ ->
      let left, right = divide list in
      let (), () =
        Parallel.fork_join2 par (fun par -> loop par left) (fun par -> loop par right)
      in
      ()
  in
  loop par list;
  Atomic.get results
;;

(* This is a nice, non-deterministic test. It runs a different way every time! *)
let%expect_test _ =
  let list = List.init 10_000 ~f:Float.of_int in
  let results = run_with_par (fun par -> sqrts par list) in
  print_s [%sexp (List.take results 10 : (float * float) list)];
  [%expect
    {|
    ((8189 90.493093659129585) (4093 63.976558206893252)
     (6141 78.364532793860263) (2045 45.221676218380054) (7165 84.64632301523794)
     (3069 55.398555937858163) (5117 71.533209071032175) (9213 95.98437372822724)
     (1021 31.953090617340916) (7677 87.6184911990614))
    |}]
;;
