open! Base
open! Stdio
open! Import
module Capsule = Portable.Capsule.Expert

let%expect_test _ =
  let (P key) = Capsule.create () in
  let capsule_ref = Capsule.Data.create (fun () -> ref 0) in
  let _r1, key =
    Capsule.Key.with_password key ~f:(fun password ->
      Capsule.Data.iter capsule_ref ~password ~f:(fun ref -> ref := !ref + 1))
  in
  let _r2, key =
    Capsule.Key.with_password key ~f:(fun password ->
      Capsule.Data.iter capsule_ref ~password ~f:(fun ref -> ref := !ref - 1))
  in
  ignore key
;;

module Par_array = Parallel.Arrays.Array
module Slice = Parallel.Arrays.Array.Slice

let swap slice ~i ~j =
  let temp = Slice.get slice i in
  Slice.set slice i (Slice.get slice j);
  Slice.set slice j temp
;;

let partition slice =
  let length = Slice.length slice in
  let pivot = Random.int length in
  swap slice ~i:pivot ~j:(length - 1);
  let pivot = Slice.get slice (length - 1) in
  let store = ref 0 in
  for i = 0 to length - 2 do
    if Slice.get slice i <= pivot
    then (
      swap slice ~i ~j:!store;
      Int.incr store)
  done;
  swap slice ~i:!store ~j:(length - 1);
  !store
;;

let rec quicksort slice =
  if Slice.length slice > 1
  then (
    let pivot = partition slice in
    let length = Slice.length slice in
    let left = Slice.sub slice ~i:0 ~j:pivot in
    let right = Slice.sub slice ~i:pivot ~j:length in
    quicksort left;
    quicksort right [@nontail])
;;

let () = ignore quicksort

(*
   let rec quicksort parallel slice =
  if Slice.length slice > 1
  then (
    let pivot = partition slice in
    let length = Slice.length slice in
    let left = Slice.sub slice ~i:0 ~j:pivot in
    let right = Slice.sub slice ~i:pivot ~j:length in
    let (), () =
      Parallel.fork_join2
        parallel
        (fun parallel -> quicksort parallel left)
        (fun parallel -> quicksort parallel right)
    in
    ())
;;
*)
