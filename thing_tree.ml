open! Base
open! Stdio

module Thing = struct
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
  let rec total_and_count par tree : total:float * count:int =
    match (tree : Thing.t Tree.t) with
    | Leaf thing -> ~total:thing.price, ~count:1
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
