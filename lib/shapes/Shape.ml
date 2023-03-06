type t = Sphere of Sphere.t

let hit shape ?min_t ?max_t ray =
  match shape with Sphere sphere -> Sphere.hit sphere ?min_t ?max_t ray
