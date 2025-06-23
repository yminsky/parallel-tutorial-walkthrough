open! Core
open! Import
module Capsule = Portable.Capsule.Expert
module Par_array = Parallel.Arrays.Array
module Slice = Parallel.Arrays.Array.Slice

(** This is an attempt to do an efficient merge-sort. It's not great now because it needs
    to do a lot of copying.

    TODO: we should figure out how to do this with just a single copy of the array, and
    some ping-ponging back and forth. The trick thing will be to do a fork-join on slices
    of a parallel array, in unison. *)

module Ordinary = struct
  let merge f s =
    Iarray.create
      0
      ~len:(Iarray.length f + Iarray.length s)
      ~mutate:(fun dest ->
        let rec loop ~f_idx ~s_idx ~dest_idx =
          let f_done = f_idx >= Iarray.length f in
          let s_done = s_idx >= Iarray.length s in
          let take_from_f () =
            dest.(dest_idx) <- Iarray.get f f_idx;
            loop ~f_idx:(f_idx + 1) ~s_idx ~dest_idx:(dest_idx + 1)
          in
          let take_from_s () =
            dest.(dest_idx) <- Iarray.get s s_idx;
            loop ~f_idx ~s_idx:(s_idx + 1) ~dest_idx:(dest_idx + 1)
          in
          if f_done && s_done
          then ()
          else if f_done
          then take_from_s () [@nontail]
          else if s_done
          then take_from_f () [@nontail]
          else if Iarray.get f f_idx < Iarray.get s s_idx
          then take_from_f () [@nontail]
          else take_from_s () [@nontail]
        in
        loop ~f_idx:0 ~s_idx:0 ~dest_idx:0 [@nontail])
  ;;

  let rec mergesort array =
    match Iarray.length array with
    | 0 | 1 -> array
    | _ ->
      let half = Iarray.length array / 2 in
      let first = Iarray.sub array ~pos:0 ~len:half in
      let second = Iarray.sub array ~pos:half ~len:(Iarray.length array - half) in
      merge (mergesort first) (mergesort second)
  ;;

  let%expect_test _ =
    let state = Random.State.make [| 1; 2; 3 |] in
    let ar = Iarray.init 50 ~f:(fun (_ : int) -> Random.State.int state 1000) in
    print_s [%sexp (ar : int iarray)];
    [%expect
      {|
    (419 900 549 117 930 769 320 250 769 916 842 525 625 929 278 649 127 734 44
     362 989 317 935 687 576 189 665 691 730 519 914 658 477 504 701 203 410 177
     372 598 841 139 942 827 296 50 201 18 886 996)
    |}];
    print_s [%sexp (mergesort ar : int iarray)];
    [%expect
      {|
    (18 44 50 117 127 139 177 189 201 203 250 278 296 317 320 362 372 410 419 477
     504 519 525 549 576 598 625 649 658 665 687 691 701 730 734 769 769 827 841
     842 886 900 914 916 929 930 935 942 989 996)
    |}]
  ;;
end

module Par_simple = struct
  let merge (f : int iarray) (s : int iarray) =
    Iarray.create
      0
      ~len:(Iarray.length f + Iarray.length s)
      ~mutate:(fun dest ->
        let rec loop ~f_idx ~s_idx ~dest_idx =
          let f_done = f_idx >= Iarray.length f in
          let s_done = s_idx >= Iarray.length s in
          let take_from_f () =
            dest.(dest_idx) <- Iarray.get f f_idx;
            loop ~f_idx:(f_idx + 1) ~s_idx ~dest_idx:(dest_idx + 1)
          in
          let take_from_s () =
            dest.(dest_idx) <- Iarray.get s s_idx;
            loop ~f_idx ~s_idx:(s_idx + 1) ~dest_idx:(dest_idx + 1)
          in
          if f_done && s_done
          then ()
          else if f_done
          then take_from_s () [@nontail]
          else if s_done
          then take_from_f () [@nontail]
          else if Iarray.get f f_idx < Iarray.get s s_idx
          then take_from_f () [@nontail]
          else take_from_s () [@nontail]
        in
        loop ~f_idx:0 ~s_idx:0 ~dest_idx:0 [@nontail])
  ;;

  let mergesort par (array : int iarray) ~seq_thresh : int iarray =
    let rec loop par array : int iarray =
      if Iarray.length array <= seq_thresh
      then Ordinary.mergesort array
      else (
        match Iarray.length array with
        | 0 | 1 -> array
        | _ ->
          let half = Iarray.length array / 2 in
          let first, second =
            Parallel.fork_join2
              par
              (fun par ->
                let first = Iarray.sub array ~pos:0 ~len:half in
                loop par first)
              (fun par ->
                let second =
                  Iarray.sub array ~pos:half ~len:(Iarray.length array - half)
                in
                loop par second)
          in
          merge first second)
    in
    loop par array
  ;;

  let%expect_test _ =
    let state = Random.State.make [| 1; 2; 3 |] in
    let ar = Iarray.init 50 ~f:(fun (_ : int) -> Random.State.int state 1000) in
    print_s [%sexp (ar : int iarray)];
    [%expect
      {|
    (419 900 549 117 930 769 320 250 769 916 842 525 625 929 278 649 127 734 44
     362 989 317 935 687 576 189 665 691 730 519 914 658 477 504 701 203 410 177
     372 598 841 139 942 827 296 50 201 18 886 996)
    |}];
    let result =
      Par_ctx.run (Par_ctx.create ()) (fun par -> mergesort par ar ~seq_thresh:1)
    in
    print_s [%sexp (result : int iarray)];
    [%expect
      {|
      Domains: [default]\n
      (18 44 50 117 127 139 177 189 201 203 250 278 296 317 320 362 372 410 419 477
       504 519 525 549 576 598 625 649 658 665 687 691 701 730 734 769 769 827 841
       842 886 900 914 916 929 930 935 942 989 996)
      |}]
  ;;
end

(* CR yminsky: Fix this. it's clearly busted for a small type thing I don't understand. *)
(* Benchmarks *)
(*

module%bench Merge_sort = struct
  let random_iarray size =
    let state = Random.State.make [| 1; 2; 3; 4; 5 |] in
    Iarray.init size ~f:(fun _ -> Random.State.int state 10_000_000_000)
  ;;

  let sizes = [ 100; 1_000; 10_000; 100_000; 1_000_000; 10_000_000 ]

  let%bench_fun ("ordinary" [@indexed size = sizes]) =
    let ar = random_iarray size in
    fun () -> Ordinary.mergesort ar
  ;;

  let ctx = Par_ctx.create ()
  let seq_thresh = [ 1; 10; 100; 1000; 10_000 ]

  let sizes_and_thresholds =
    List.cartesian_product sizes seq_thresh
    |> List.map ~f:(fun (size, seq_thresh) ->
      let label = sprintf "size: %d, seq_thresh: %d" size seq_thresh in
      label, (~size, ~seq_thresh))
  ;;

     let%bench_fun ("parallel" [@params size_and_threshold = sizes_and_thresholds]) =
    let ~size, ~seq_thresh = size_and_threshold in
    let ar = random_iarray size in
    fun () -> Par_ctx.run ctx (fun par -> Par_simple.mergesort par ar ~seq_thresh)
  ;;
end
  *)
