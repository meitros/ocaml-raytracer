open! Core

(* generate a point that's within a unit sphere *)
let rec random_point_in_unit_sphere () =
  let point = Vec3.random ~min:(-1.) 1. in
  if Float.(Vec3.length_squared point >= 1.) then random_point_in_unit_sphere ()
  else point
