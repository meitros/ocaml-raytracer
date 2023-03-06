open! Core
open BaseTypes

type scatter_result = Absorb | Scatter of (Ray.t * color)

let diffuse_scatter ray hit_details shape_color =
  let scatter_direction' =
    Vec3.(
      hit_details.normal + (unit @@ MathUtils.random_point_in_unit_sphere ()))
  in
  let scatter_direction =
    if Vec3.near_zero scatter_direction' then hit_details.normal
    else scatter_direction'
  in
  Scatter (Ray.create hit_details.point scatter_direction, shape_color)

let metal_scatter (ray : Ray.t) (hit_details : shape_hit_details)
    (shape_color : color) (fuzziness : float) =
  let reflected = Vec3.(reflect (unit ray.direction) hit_details.normal) in
  let scattered =
    Ray.create hit_details.point
      Vec3.(
        reflected
        + Vec3.scale (MathUtils.random_point_in_unit_sphere ()) fuzziness)
  in
  let should_reflect =
    Float.(Vec3.dot scattered.direction hit_details.normal > 0.)
  in
  if should_reflect then Scatter (scattered, shape_color) else Absorb

let scatter ray hit_details material =
  match material with
  | Diffuse color -> diffuse_scatter ray hit_details color
  | Metal { color; fuzziness } -> metal_scatter ray hit_details color fuzziness
