open! Core
open! Import
module Capsule = Portable.Capsule.Expert
module Par_array = Parallel.Arrays.Array
module Slice = Parallel.Arrays.Array.Slice

module Ordinary = struct
  let merge f s =
    let dest = Array.create ~len:(Array.length f + Array.length s) 0 in
    let rec loop ~f_idx ~s_idx ~dest_idx =
      let f_done = f_idx >= Array.length f in
      let s_done = s_idx >= Array.length s in
      let take_from_f () =
        dest.(dest_idx) <- f.(f_idx);
        loop ~f_idx:(f_idx + 1) ~s_idx ~dest_idx:(dest_idx + 1)
      in
      let take_from_s () =
        dest.(dest_idx) <- s.(s_idx);
        loop ~f_idx ~s_idx:(s_idx + 1) ~dest_idx:(dest_idx + 1)
      in
      if f_done && s_done
      then ()
      else if f_done
      then take_from_s ()
      else if s_done
      then take_from_f ()
      else if f.(f_idx) < s.(s_idx)
      then take_from_f ()
      else take_from_s ()
    in
    loop ~f_idx:0 ~s_idx:0 ~dest_idx:0;
    dest
  ;;

  let rec mergesort array =
    match Array.length array with
    | 0 | 1 -> Array.copy array
    | _ ->
      let half = Array.length array / 2 in
      let first = Array.sub array ~pos:0 ~len:half in
      let second = Array.sub array ~pos:half ~len:(Array.length array - half) in
      merge (mergesort first) (mergesort second)
  ;;

  let%expect_test _ =
    let state = Random.State.make [| 1; 2; 3 |] in
    let ar = Array.init 50 ~f:(fun (_ : int) -> Random.State.int state 1000) in
    print_s [%sexp (ar : int array)];
    [%expect
      {|
    (419 900 549 117 930 769 320 250 769 916 842 525 625 929 278 649 127 734 44
     362 989 317 935 687 576 189 665 691 730 519 914 658 477 504 701 203 410 177
     372 598 841 139 942 827 296 50 201 18 886 996)
    |}];
    print_s [%sexp (mergesort ar : int array)];
    [%expect
      {|
    (18 44 50 117 127 139 177 189 201 203 250 278 296 317 320 362 372 410 419 477
     504 519 525 549 576 598 625 649 658 665 687 691 701 730 734 769 769 827 841
     842 886 900 914 916 929 930 935 942 989 996)
    |}]
  ;;
end

module Par = struct
  let merge (f : int iarray) (s : int iarray) =
    let dest = Array.init (Iarray.length f + Iarray.length s) ~f:(fun _ -> 0) in
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
      then take_from_s ()
      else if s_done
      then take_from_f ()
      else if Iarray.get f f_idx < Iarray.get s s_idx
      then take_from_f ()
      else take_from_s ()
    in
    loop ~f_idx:0 ~s_idx:0 ~dest_idx:0;
    (* Safe, since we'll never have access to the mutable array after this point *)
    Iarray.unsafe_of_array__promise_no_mutation dest
  ;;

  let rec mergesort par (array : int iarray) : int iarray =
    match Iarray.length array with
    | 0 | 1 -> array
    | _ ->
      let half = Iarray.length array / 2 in
      let first, second =
        Parallel.fork_join2
          par
          (fun par ->
            let first = Iarray.sub array ~pos:0 ~len:half in
            mergesort par first)
          (fun par ->
            let second = Iarray.sub array ~pos:half ~len:(Iarray.length array - half) in
            mergesort par second)
      in
      merge first second
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
    print_s [%sexp (run_with_par (fun par -> mergesort par ar) : int iarray)];
    [%expect
      {|
    (18 44 50 117 127 139 177 189 201 203 250 278 296 317 320 362 372 410 419 477
     504 519 525 549 576 598 625 649 658 665 687 691 701 730 734 769 769 827 841
     842 886 900 914 916 929 930 935 942 989 996)
    |}]
  ;;
end
