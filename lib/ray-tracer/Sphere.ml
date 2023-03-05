open! Base
open BaseTypes

type t = { center : point; radius : float }

let create center radius : t = { center; radius }

let hit ({ center; radius } : t) ?min_t ?max_t (ray : Ray.t) : object_hit =
  let filter_roots_by_args (roots : float list) =
    (* given a list of roots, make sure they're within the min/max range above *)
    roots
    |> List.filter ~f:(fun r ->
           match min_t with None -> true | Some min_t' -> Float.(r >= min_t'))
    |> List.filter ~f:(fun r ->
           match max_t with None -> true | Some max_t' -> Float.(r <= max_t'))
  in
  let calculate_roots () : float list =
    (* we have an equation to find the values of t that intersect the surface
       of the sphere. we return them nearest to farthest *)
    let oc = Vec3.(ray.start - center) in
    let a = Vec3.length_squared ray.direction in
    let half_b = Vec3.dot oc ray.direction in
    let c = Vec3.length_squared oc -. Float.square radius in
    let discriminant = Float.square half_b -. (a *. c) in
    if Float.(discriminant < 0.) then []
    else
      let sqrt_discriminant = Float.sqrt discriminant in
      [
        ((-1. *. half_b) -. sqrt_discriminant) /. a;
        ((-1. *. half_b) +. sqrt_discriminant) /. a;
      ]
  in
  let valid_roots = filter_roots_by_args @@ calculate_roots () in
  (* roots are ordered from nearest to furthest, we want the first one that's
     valid *)
  match valid_roots with
  | [] -> Miss
  | root :: _ ->
      let point = Ray.at ray root in
      let outward_normal = Vec3.scale_div Vec3.(point - center) radius in
      let is_front_face = Float.(Vec3.dot ray.direction outward_normal < 0.) in
      Hit
        {
          point;
          normal =
            (if is_front_face then outward_normal else Vec3.neg outward_normal);
          is_front_face;
          t = root;
        }
