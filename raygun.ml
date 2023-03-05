open! Core
open Raygun_lib
open BaseTypes

let print_vec vec = Stdio.print_endline @@ Vec3.to_s vec

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
  let sphere = Sphere.create (0., 0., -1.) 0.5 in
  match Sphere.hit sphere ray with
  | Miss -> bg_color ray
  | Hit { t; _ } ->
      let n = Vec3.(unit (Ray.at ray t) - create 0. 0. (-1.)) in
      Vec3.scale (Vec3.add_scal n 1.) 0.5

let write_image image filename =
  let out = Caml.open_out filename in
  Caml.Out_channel.output_string out (ImageData.to_ppm image);
  ignore (Caml.close_out out)

let () =
  let aspect_ratio = 16. /. 9. in
  let image_width = 400 in
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
