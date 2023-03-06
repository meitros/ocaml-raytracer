open! Base
open BaseTypes

(* A simple camera
   / always located at (0,0,0)
   / the image is created from a viewport at z=-1, with fixed height 2
     and width based on the provided aspect ratio.
*)

type t = {
  origin : point;
  viewport_height : float;
  viewport_width : float;
  focal_length : float;
  horizontal : Vec3.t;
  vertical : Vec3.t;
  lower_left_corner : point;
}

let create aspect_ratio : t =
  let origin = (0., 0., 0.) in
  let viewport_height = 2. in
  let viewport_width = aspect_ratio *. viewport_height in
  let focal_length = 1. in
  let horizontal = Vec3.create viewport_width 0. 0. in
  let vertical = Vec3.create 0. viewport_height 0. in
  {
    origin;
    viewport_height;
    viewport_width;
    focal_length;
    horizontal;
    vertical;
    lower_left_corner =
      Vec3.(
        origin - scale horizontal 0.5 - scale vertical 0.5
        - create 0. 0. focal_length);
  }
