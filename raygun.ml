open! Core
open Raygun_lib

let print_vec vec = Stdio.print_endline @@ Vec3.to_s vec

(* 
 * Create a vertical linear gradient background color
 * (blending (1,1,1) and (0.5, 0.7. 1.0)) 
 *)

let write_image image filename =
  let out = Caml.open_out filename in
  Caml.Out_channel.output_string out (ImageData.to_ppm image);
  ignore (Caml.close_out out)

let scene_definition =
  let open Shape in
  let sphere1 = Sphere (Sphere.create (0., 0., -1.) 0.5) in
  let sphere2 = Sphere (Sphere.create (0., -100.5, -1.) 100.) in
  [ sphere1; sphere2 ]

let () =
  let aspect_ratio = 16. /. 9. in
  let camera = FixedCamera.create aspect_ratio in
  let scene = Scene.create ~aspect_ratio ~scene_definition ~camera in
  let image = Scene.render scene ~image_width:400 in
  write_image image "test.ppm"
