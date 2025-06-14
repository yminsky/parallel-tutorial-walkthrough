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

(*
   (* This is a nice, non-deterministic test. It runs a different way every time! *)
let%expect_test _ =
  let list = List.init 10_000 ~f:Float.of_int in
  let results = run_with_par (fun par -> sqrts par list) in
  print_s [%sexp (List.take results 10 : (float * float) list)];
  [%expect
    {|
    ((7227 85.011763891828522) (3131 55.955339334151127)
     (7923 89.011235245894667) (3827 61.862751312886175)
     (5875 76.648548583779458) (9971 99.8548947222919) (1779 42.178193417926281)
     (8055 89.749651809909551) (3959 62.920584866957491) (8183 90.4599358832406))
    |}]
;;
*)
