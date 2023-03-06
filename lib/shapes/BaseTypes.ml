open! Base

(* Math *)
type point = Vec3.t
type color = Vec3.t

(* Intersections types *)

(* The details of a ray hitting an object *)
type shape_hit_details = {
  point : Vec3.t;
  (* 
   * we have the normal always oppose the ray, e.g it points out when 
   * hitting a sphere from the outside and points in when we're inside 
   * the sphere. This means we need is_front_face to disambiguate 
   *)
  normal : Vec3.t;
  is_front_face : bool;
  t : float;
}

type shape_hit = Hit of shape_hit_details | Miss

(* Materials Types *)

type material =
  | Diffuse of color
  | Metal of { color : color; fuzziness : float }
