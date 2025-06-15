open! Base
open! Stdio

type 'a t =
  | Leaf of 'a
  | Node of 'a t * 'a t
[@@deriving sexp]
