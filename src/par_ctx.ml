open! Base
open! Stdio
module Scheduler = Parallel_scheduler_work_stealing

(*
   let run_with_par f =
  let scheduler = Scheduler.create () in
  let monitor = Parallel.Monitor.create_root () in
  let result = Scheduler.schedule scheduler ~monitor ~f in
  Scheduler.stop scheduler;
  result
;;
*)

type t =
  { scheduler : Scheduler.t
  ; monitor : Parallel.Monitor.t
  }

let create ?domains () =
  let scheduler = Scheduler.create ?domains () in
  let monitor = Parallel.Monitor.create_root () in
  let domains_s =
    Option.map ~f:Int.to_string domains |> Option.value ~default:"[default]"
  in
  print_endline [%string {|Domains: %{domains_s}\n|}];
  { scheduler; monitor }
;;

let run t f =
  let result = Scheduler.schedule t.scheduler ~monitor:t.monitor ~f in
  (* Scheduler.stop ctx.scheduler;*)
  (* CR don't really know what stops do, and if they can be
     undone. Will consider it later. *)
  result
;;
