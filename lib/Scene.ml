open! Core

type t = {
  aspect_ratio : float;
  scene_definition : Shape.t list;
  camera : FixedCamera.t;
}

let create ~aspect_ratio ~scene_definition ~camera =
  { aspect_ratio; scene_definition; camera }

let get_nearest_hit scene (ray : Ray.t) =
  List.fold scene.scene_definition ~init:None ~f:(fun curr shape ->
      match Shape.hit shape ~min_t:0. ray with
      | Miss -> curr
      | Hit hit_details -> (
          match curr with
          | None -> Some (shape, hit_details)
          | Some (last_shape, last_hit) ->
              if Float.(hit_details.t < last_hit.t) then
                Some (shape, hit_details)
              else Some (last_shape, last_hit)))

let color_of_ray scene (ray : Ray.t) =
  match get_nearest_hit scene ray with
  | None -> Utils.bg_color ray
  | Some (shape, { normal; t; _ }) ->
      Vec3.scale Vec3.(normal + Vec3.create 1. 1. 1.) 0.5

let render (scene : t) ~image_width =
  let camera = scene.camera in
  let image_height =
    Int.of_float @@ (Float.of_int image_width /. scene.aspect_ratio)
  in
  let render_pixel x y =
    let open Float in
    let u = of_int x /. (of_int image_width -. 1.) in
    let v = of_int y /. (of_int image_height -. 1.) in
    let r : Ray.t =
      {
        start = camera.origin;
        direction =
          Vec3.(
            camera.lower_left_corner + scale camera.horizontal u
            + scale camera.vertical v - camera.origin);
      }
    in
    let c = color_of_ray scene r in
    ImageData.color_to_pixel c
  in
  ImageData.init image_width image_height ~f:render_pixel
