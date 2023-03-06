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

let render ?(samples_per_pixel = 75) (scene : t) ~image_width =
  let camera = scene.camera in
  let image_height =
    Int.of_float @@ (Float.of_int image_width /. scene.aspect_ratio)
  in
  let render_pixel_helper x y =
    let open Float in
    (*
       we anti-alias if samples_per_pixel > 1 - think of a pixel as a small
       square area within the viewport, and we send multiple rays through that
       square and average the resulting colors.

       (this also keeps the property that if AA is disabled, the image is always
       identical)
    *)
    let x_jittered =
      if Int.(samples_per_pixel = 1) then of_int x
      else of_int x +. Random.float 1.
    in
    let y_jittered =
      if Int.(samples_per_pixel = 1) then of_int y
      else of_int y +. Random.float 1.
    in
    let u = x_jittered /. (of_int image_width -. 1.) in
    let v = y_jittered /. (of_int image_height -. 1.) in
    let r : Ray.t =
      {
        start = camera.origin;
        direction =
          Vec3.(
            camera.lower_left_corner + scale camera.horizontal u
            + scale camera.vertical v - camera.origin);
      }
    in
    color_of_ray scene r
  in
  let render_pixel x y =
    let samples =
      List.init samples_per_pixel ~f:(fun _ -> render_pixel_helper x y)
    in
    let averaged =
      Vec3.scale_div
        (List.reduce_exn samples ~f:Vec3.add)
        (Float.of_int samples_per_pixel)
    in
    ImageData.color_to_pixel averaged
  in
  ImageData.init image_width image_height ~f:render_pixel
