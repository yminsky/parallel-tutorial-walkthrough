open Base
open Import

type t =
  { scheduler : Scheduler.t
  ; monitor : Parallel.Monitor.t
  }

val create : ?domains:int -> unit -> t
val run : t -> (local_ Parallel_kernel.t -> 'a) @ portable -> 'a
