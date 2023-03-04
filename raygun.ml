open! Core

(* Represents a single pixel. Values are from 0 - 255 *)
type pixel = int * int * int

(* matrix of pixels (implemented using array) *)
module ImageData = struct
  (* the pixel data, width, and height *)
  type t = { width : int; height : int; data : pixel Array.t }

  let create width height : t =
    { width; height; data = Array.create ~len:(width * height) (0, 0, 0) }

  let init width height ~f : t =
    {
      width;
      height;
      data =
        Array.init (width * height) ~f:(fun i -> f (i % width) (i / height));
    }

  (* sets a pixel in the image *)
  let set_exn (image : t) x y (color : pixel) =
    if x >= image.width then raise_s [%message "out of bounds x"];
    if y >= image.height then raise_s [%message "out of bounds y"];
    image.data.((image.width * y) + x) <- color
end

module Vec3 = struct
  type t = float * float * float

  let create x y z : t = (x, y, z)
  let scale ((x, y, z) : t) by = (x *. by, y *. by, z *. by)
  let length (x, y, z) = Float.(sqrt (square x + square y + square z))

  let unit vec = let length = length vec in

                 scale vec (1. /. length)

  let ( + ) (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)
  let ( - ) (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)
  let ( *. ) (x1, y1, z1) (x2, y2, z2) = (x1 *. x2, y1 *. y2, z1 *. z2)

  (* let ( ** ) (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2) *)
end

type point = Vec3.t

module Ray = struct
  type t = { start : point; direction : Vec3.t }

  let create start direction = { start; direction }
  let at { start; direction } t = Vec3.(start + scale direction t)
end

(* Converts an ImageData to a .ppm file. This uses the P3 format,
   which is entirely in ascii and easy to read.

   For simplicity I just print a single tuple per line
*)
let to_ppm (image : ImageData.t) =
  let pixels = Array.to_list image.data in
  let pixel_to_string ((r, g, b) : pixel) = Printf.sprintf "%d %d %d" r g b in
  let image_data = List.map pixels ~f:pixel_to_string in
  let lines =
    "P3"
    :: Printf.sprintf "%d %d" image.width image.height
    :: "255" :: image_data
  in
  String.concat ~sep:"\n" lines

let write_image image filename =
  let out = Caml.open_out filename in
  Caml.Out_channel.output_string out (to_ppm image);
  ignore (Caml.close_out out)

(* module Scene = struct
     type t = {
       width: int;
       height; int;
     }

     let create w h =
   end *)

let choose_color ({ direction; _ } : Ray.t) =
  let ux, uy, uz = Vec3.unit direction in
  let t = (uy *. 0.5) +. 1. in
  Vec3.(
    scale (Vec3.create 1. 1. 1.) (1. -. t) + scale (Vec3.create 0.5 0.7 1.0) t)

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
  let some_func x y' =
    let y = height - y' in
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
  let image = ImageData.init width height ~f:some_func in
  write_image image "test.ppm"
