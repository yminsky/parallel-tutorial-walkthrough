open! Base
open! Stdio
module Scheduler = Parallel_scheduler_work_stealing

let run_with_par f =
  let scheduler = Scheduler.create () in
  let monitor = Parallel.Monitor.create_root () in
  let result = Scheduler.schedule scheduler ~monitor ~f in
  Scheduler.stop scheduler;
  result
;;

(** The basic information required to run a parlalel job *)
module Run_ctx = struct
  type t =
    { scheduler : Scheduler.t
    ; monitor : Parallel.Monitor.t
    }

  let create () =
    let scheduler = Scheduler.create () in
    let monitor = Parallel.Monitor.create_root () in
    { scheduler; monitor }
  ;;
end

let par_run (ctx : Run_ctx.t) f =
  let result = Scheduler.schedule ctx.scheduler ~monitor:ctx.monitor ~f in
  (* Scheduler.stop ctx.scheduler;*)
  (* CR don't really know what stops do. Should consider it later. *)
  result
;;
