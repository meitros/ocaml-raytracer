open! Base

type point = Vec3.t

(* Return value when we test whether a ray hit an object *)
type object_hit_details = {
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

type object_hit = Hit of object_hit_details | Miss
