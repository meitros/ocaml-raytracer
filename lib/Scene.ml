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
      match Shape.hit shape ~min_t:0.00001 ray with
      | Miss -> curr
      | Hit hit_details -> (
          match curr with
          | None -> Some (shape, hit_details)
          | Some (last_shape, last_hit) ->
              if Float.(hit_details.t < last_hit.t) then
                Some (shape, hit_details)
              else Some (last_shape, last_hit)))

(* Casts a ray in `scene`, returning the color. Recursive, since the ray might
   reflect/refract from what it hits. *)
let rec color_of_ray scene (ray : Ray.t) depth =
  if depth <= 0 then (0., 0., 0.)
  else
    match get_nearest_hit scene ray with
    | None -> Utils.bg_color ray
    | Some (shape, hit_details) -> (
        match Materials.scatter ray hit_details (Shape.material shape) with
        | Absorb -> (0., 0., 0.)
        | Scatter (scatter_ray, color) ->
            Vec3.pairwise_mult color
              (color_of_ray scene scatter_ray (depth - 1)))

let render ?(samples_per_pixel = 25) (scene : t) ~image_width =
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
    color_of_ray scene r 50
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
    ImageData.color_to_pixel @@ Vec3.sqrt_exn averaged
  in
  ImageData.init image_width image_height ~f:render_pixel
