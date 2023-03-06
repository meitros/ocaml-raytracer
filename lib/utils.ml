let bg_color (ray : Ray.t) =
  let ux, uy, uz = Vec3.unit ray.direction in
  (* -1 <= uy <= 1, let's transform into 0 <= t <= 1 *)
  let t = 0.5 *. (uy +. 1.) in
  Vec3.(
    scale (Vec3.create 1. 1. 1.) (1. -. t) + scale (Vec3.create 0.5 0.7 1.0) t)
