open! Core
open Raygun_lib

type point = Vec3.t

module Ray = struct
  type t = { start : point; direction : Vec3.t }

  let create start direction = { start; direction }
  let at { start; direction } t = Vec3.(start + scale direction t)
end

let hit_sphere (center : point) (radius : float) (r : Ray.t) : bool =
  let oc = Vec3.(r.start - center) in
  let a = Vec3.dot r.direction r.direction in
  let b = 2.0 *. Vec3.dot oc r.direction in
  let c = Vec3.dot oc oc -. (radius *. radius) in
  let discriminant = (b *. b) -. (4. *. a *. c) in
  Float.(discriminant > 0.)

let choose_color (ray : Ray.t) =
  if hit_sphere (0., 0., -1.) 0.5 ray then (1.0, 0., 0.)
  else
    let ux, uy, uz = Vec3.unit ray.direction in
    let t = (uy *. 0.5) +. 1. in
    Vec3.(
      scale (Vec3.create 1. 1. 1.) (1. -. t) + scale (Vec3.create 0.5 0.7 1.0) t)

let write_image image filename =
  let out = Caml.open_out filename in
  Caml.Out_channel.output_string out (ImageData.to_ppm image);
  ignore (Caml.close_out out)

let () =
  let aspect_ratio = 16. /. 9. in
  let width = 400 in
  let height = Int.of_float @@ (Float.of_int width /. aspect_ratio) in
  let viewport_height = 2. in
  let viewport_width = aspect_ratio *. viewport_height in
  let focal_length = 1. in
  let origin = Vec3.create 0. 0. 0. in
  let horizontal = Vec3.create viewport_width 0. 0. in
  let vertical = Vec3.create 0. viewport_height 0. in
  let lower_left_corner =
    Vec3.(
      origin - scale horizontal 0.5 - scale vertical 0.5
      - create 0. 0. focal_length)
  in
  let init_f x y =
    let open Float in
    let u = of_int x /. (of_int width -. 1.) in
    let v = of_int y /. (of_int height -. 1.) in
    let r : Ray.t =
      {
        start = origin;
        direction =
          Vec3.(
            lower_left_corner + scale horizontal u + scale vertical v - origin);
      }
    in
    let cx, cy, cz = choose_color r in
    Int.(of_float (255. *. cx), of_float (255. *. cy), of_float (255. *. cz))
  in
  let image = ImageData.init width height ~f:init_f in
  write_image image "test.ppm"
