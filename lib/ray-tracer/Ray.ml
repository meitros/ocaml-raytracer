open! Base
open BaseTypes

(* We think of rays as start + t * direction, where t is time *)

type t = { start : point; direction : Vec3.t }

let create start direction = { start; direction }
let at { start; direction } t = Vec3.(start + scale direction t)
