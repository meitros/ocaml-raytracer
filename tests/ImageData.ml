open Raygun_lib
open Stdio

(*
Tiny ppm tests. The ppm I'll generate (top-left green, top-right blue, 
bottom-left grey, bottom-right white):

P3
2 2
255
0 255 0
0 0 255
127 127 127
255 255 255

*)

let%expect_test "create" =
  let create x y =
    if x = 0 && y = 0 then (0, 255, 0)
    else if x = 1 && y = 0 then (0, 0, 255)
    else if x = 0 && y = 1 then (127, 127, 127)
    else (255, 255, 255)
  in
  let image = ImageData.init 2 2 ~f:create in
  print_endline @@ ImageData.to_ppm image;
  [%expect {|
    P3
    2 2
    255
    0 255 0
    0 0 255
    127 127 127
    255 255 255 |}]

let%expect_test "set_pixel" =
  let image = ImageData.create 2 2 in
  ImageData.set_pixel_exn image 0 0 (0, 255, 0);
  ImageData.set_pixel_exn image 1 0 (0, 0, 255);
  ImageData.set_pixel_exn image 0 1 (127, 127, 127);
  ImageData.set_pixel_exn image 1 1 (255, 255, 255);
  print_endline @@ ImageData.to_ppm image;
  [%expect {|
    P3
    2 2
    255
    0 255 0
    0 0 255
    127 127 127
    255 255 255 |}]

let%expect_test "set" =
  let image = ImageData.create 2 2 in
  ImageData.set_exn image 0 0 (0., 1., 0.);
  ImageData.set_exn image 1 0 (0., 0., 1.);
  ImageData.set_exn image 0 1 (0.5, 0.5, 0.5);
  ImageData.set_exn image 1 1 (1., 1., 1.);
  print_endline @@ ImageData.to_ppm image;
  [%expect {|
    P3
    2 2
    255
    0 255 0
    0 0 255
    127 127 127
    255 255 255 |}]
