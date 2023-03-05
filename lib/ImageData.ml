open! Core

(* matrix of pixels (implemented using array). coordinate-system wise, 
 * (0, 0) is the lower-left corner 
 *)

(* Represents a single pixel. Values are from 0 - 255 *)
type pixel = int * int * int

(* the pixel data, width, and height *)
type t = { width : int; height : int; data : pixel Array.t }

(* Creates a new image data, with each pixel initialized to black *)
let create width height : t =
  { width; height; data = Array.create ~len:(width * height) (0, 0, 0) }

(* Creates a new image data, calling f x y to initialize each pixel *)
let init width height ~f : t =
  {
    width;
    height;
    data =
      Array.init (width * height) ~f:(fun i ->
          f (i % width) (height - (i / width) - 1));
  }

(* converts `(rf, gf, bf)` to `(r,g,b)` (floats from 0-1 to ints from 0-255) *)
let color_to_pixel (rf, gf, bf) =
  Int.(of_float (255. *. rf), of_float (255. *. gf), of_float (255. *. bf))

(* sets a pixel in the image. Each pixel color should be between 0-255 *)
let set_pixel_exn (image : t) x y (color : pixel) =
  if x < 0 || x >= image.width then raise_s [%message "out of bounds x"];
  if y < 0 || y >= image.height then raise_s [%message "out of bounds y"];
  (* (0, 0) is lower left so we need to flip y *)
  let y' = image.height - 1 - y in
  image.data.((image.width * y') + x) <- color

(* sets the color of the `(x, y)` pixel. `(rf, gf, bf)` are rgb components
   that range from 0-1 *)
let set_exn (image : t) x y (rf, gf, bf) =
  set_pixel_exn image x y (color_to_pixel (rf, gf, bf))

(* Converts an ImageData to a .ppm file. This uses the P3 format,
   which is entirely in ascii and easy to read.

   For simplicity I just print a single tuple per line
*)
let to_ppm (image : t) =
  let pixels = Array.to_list image.data in
  let pixel_to_string ((r, g, b) : pixel) = Printf.sprintf "%d %d %d" r g b in
  let image_data = List.map pixels ~f:pixel_to_string in
  let lines =
    "P3"
    :: Printf.sprintf "%d %d" image.width image.height
    :: "255" :: image_data
  in
  (* this format breaks with some viewers if there's no ending newline *)
  String.concat ~sep:"\n" lines ^ "\n"
