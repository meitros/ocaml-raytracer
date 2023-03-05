open! Core
open Raygun_lib

type point = Vec3.t

let print_vec vec = Stdio.print_endline @@ Vec3.to_s vec

module Ray = struct
  (*
     We think of rays as start + t * direction, where t is time
  *)
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

(* 
 * Create a vertical linear gradient background color
 * (blending (1,1,1) and (0.5, 0.7. 1.0)) 
 *)
let bg_color (ray : Ray.t) =
  let ux, uy, uz = Vec3.unit ray.direction in
  (* -1 <= uy <= 1, let's transform into 0 <= t <= 1 *)
  let t = 0.5 *. (uy +. 1.) in
  Vec3.(
    scale (Vec3.create 1. 1. 1.) (1. -. t) + scale (Vec3.create 0.5 0.7 1.0) t)

let choose_color (ray : Ray.t) =
  if hit_sphere (0., 0., -1.) 0.5 ray then (1.0, 0., 0.) else bg_color ray

let write_image image filename =
  let out = Caml.open_out filename in
  Caml.Out_channel.output_string out (ImageData.to_ppm image);
  ignore (Caml.close_out out)

let () =
  let aspect_ratio = 16. /. 9. in
  let image_width = 64 in
  let image_height =
    Int.of_float @@ (Float.of_int image_width /. aspect_ratio)
  in
  (* camera / viewport. uses a right-handed coordinate system with
     into the screen being -z *)
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
  let render_pixel x y =
    let open Float in
    let u = of_int x /. (of_int image_width -. 1.) in
    let v = of_int y /. (of_int image_height -. 1.) in
    let r : Ray.t =
      {
        start = origin;
        direction =
          Vec3.(
            lower_left_corner + scale horizontal u + scale vertical v - origin);
      }
    in
    let c = choose_color r in
    ImageData.color_to_pixel c
  in
  let image = ImageData.init image_width image_height ~f:render_pixel in
  write_image image "test.ppm"
