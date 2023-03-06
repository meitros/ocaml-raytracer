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

(* let simple_scene_definition =
   let open Shape in
   let sphere1 =
     Sphere (Sphere.create (0., 0., -1.) 0.5 (Diffuse (0.5, 0.7, 0.9)))
   in
   let sphere2 =
     Sphere (Sphere.create (0., -100.5, -1.) 100. (Diffuse (0.5, 0.7, 0.9)))
   in
   [ sphere1; sphere2 ] *)

(* auto material_ground = make_shared<lambertian>(color(0.8, 0.8, 0.0));
   auto material_center = make_shared<lambertian>(color(0.7, 0.3, 0.3));
   auto material_left   = make_shared<metal>(color(0.8, 0.8, 0.8));
   auto material_right  = make_shared<metal>(color(0.8, 0.6, 0.2));

   world.add(make_shared<sphere>(point3( 0.0, -100.5, -1.0), 100.0, material_ground));
   world.add(make_shared<sphere>(point3( 0.0,    0.0, -1.0),   0.5, material_center));
   world.add(make_shared<sphere>(point3(-1.0,    0.0, -1.0),   0.5, material_left));
   world.add(make_shared<sphere>(point3( 1.0,    0.0, -1.0),   0.5, material_right));
*)
let four_spheres_definition =
  let open Shape in
  let spheres =
    [
      (* ground *)
      Sphere.create (0., -100.5, -1.) 100. (Diffuse (0.8, 0.8, 0.));
      (* center *)
      Sphere.create (0., 0., -1.) 0.5 (Diffuse (0.7, 0.3, 0.3));
      (* left *)
      Sphere.create (-1., 0., -1.) 0.5
        (Metal { color = (0.8, 0.8, 0.8); fuzziness = 0.3 });
      (* right *)
      Sphere.create (1., 0., -1.) 0.5
        (Metal { color = (0.8, 0.6, 0.2); fuzziness = 1.0 });
    ]
  in
  List.map ~f:(fun s -> Sphere s) spheres

let () =
  let aspect_ratio = 16. /. 9. in
  let camera = FixedCamera.create aspect_ratio in
  let scene =
    Scene.create ~aspect_ratio ~scene_definition:four_spheres_definition ~camera
  in
  let image = Scene.render scene ~image_width:400 in
  write_image image "test.ppm"
