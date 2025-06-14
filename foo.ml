open! Base
open! Stdio

let add4 par a b c d =
  let a_plus_b, c_plus_d = Parallel.fork_join2 par (fun _ -> a + b) (fun _ -> c + d) in
  a_plus_b + c_plus_d
;;

let test_add4 par = add4 par 1 10 100 1000

let run_one_test f =
  let module Scheduler = Parallel_scheduler_work_stealing in
  let scheduler = Scheduler.create () in
  let monitor = Parallel.Monitor.create_root () in
  let result = Scheduler.schedule scheduler ~monitor ~f in
  Scheduler.stop scheduler;
  result
;;

let%expect_test _ =
  let result = run_one_test test_add4 in
  print_s [%sexp (result : int)];
  [%expect {| 1111 |}]
;;
