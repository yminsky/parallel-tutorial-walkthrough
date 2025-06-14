open! Base
open! Stdio
open! Import

module Thing : sig @@ portable
  module Mood : sig
    type t =
      | Happy
      | Neutral
      | Sad
  end

  type t

  val create : price:float -> mood:Mood.t -> t @ portable
  val price : t @ contended -> float
  val mood : t -> Mood.t
  val cheer_up : t -> unit
end = struct
  module Mood = struct
    type t =
      | Happy
      | Neutral
      | Sad
  end

  type t =
    { price : float
    ; mutable mood : Mood.t
    }

  let create ~price ~mood = { price; mood }
  let price { price; _ } = price
  let mood { mood; _ } = mood
  let cheer_up t = t.mood <- Happy
end

let average_par par tree =
  let rec (total_and_count @ portable) par (tree @ contended) : total:float * count:int =
    match (tree : Thing.t Tree.t) with
    | Leaf thing -> ~total:(Thing.price thing), ~count:1
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

let test_thing_tree =
  let left = Tree.Leaf (Thing.create ~price:0.3 ~mood:Neutral) in
  let right = Tree.Leaf (Thing.create ~price:0.7 ~mood:Sad) in
  Tree.Node (left, right)
;;

let%expect_test _ =
  let result = run_with_par (fun par -> average_par par test_thing_tree) in
  print_s [%sexp (result : float)];
  [%expect {| 0.5 |}]
;;
