open Raygun_lib
open Stdio

let print_vec vec = print_endline @@ Vec3.to_s vec
let vec_0 = Vec3.create 1. 2. 3.
let vec_123 = Vec3.create 1. 2. 3.
let vec_321 = Vec3.create 3. 2. 1.
let vec_234 = Vec3.create 2. 3. 4.
let vec_236 = Vec3.create 2. 3. 6.
let vec_567 = Vec3.create 5. 6. 7.

(* math *)
let%expect_test "addition" =
  print_vec @@ Vec3.add vec_123 vec_567;
  print_vec Vec3.(vec_123 + vec_567);
  [%expect {|
    (6., 8., 10.)
    (6., 8., 10.) |}]

let%expect_test "add scalar" =
  print_vec @@ Vec3.add_scal vec_123 4.;
  [%expect {| (5., 6., 7.) |}]

let%expect_test "subtraction" =
  print_vec @@ Vec3.sub vec_321 vec_567;
  print_vec Vec3.(vec_321 - vec_567);
  print_vec Vec3.(vec_567 - vec_321);
  [%expect {|
    (-2., -4., -6.)
    (-2., -4., -6.)
    (2., 4., 6.) |}]

let%expect_test "negation" =
  print_vec @@ Vec3.neg vec_321;
  [%expect {| (-3., -2., -1.) |}]

let%expect_test "scale" =
  print_vec @@ Vec3.scale vec_123 3.;
  print_vec @@ Vec3.scale_div vec_123 2.;
  [%expect {|
    (3., 6., 9.)
    (0.5, 1., 1.5) |}]

let%expect_test "dot" =
  print_float @@ Vec3.dot vec_123 vec_567;
  [%expect {| 38. |}]

let%expect_test "cross" =
  print_vec @@ Vec3.cross vec_123 vec_234;
  print_vec @@ Vec3.cross vec_234 vec_123;
  [%expect {|
    (-1., 2., -1.)
    (1., -2., 1.) |}]

(* two unit vectors that point in opposite directions
   have dot product -1 *)
let%expect_test "dot_opposing_directions" =
  let vec_unit = Vec3.unit vec_123 in
  print_float @@ Vec3.dot vec_unit (Vec3.neg vec_unit);
  [%expect {| -1. |}]

(* length / unit *)

let%expect_test "length" =
  print_float @@ Vec3.length vec_236;
  [%expect {| 7. |}]

let%expect_test "unit" =
  let print_unit vec =
    print_float @@ Vec3.length @@ Vec3.unit vec;
    print_endline ""
  in
  print_unit vec_123;
  print_unit @@ Vec3.neg vec_123;
  print_unit vec_567;
  [%expect {|
    1.
    1.
    1. |}]

let%expect_test "reflect" =
  (* 45 degrees *)
  print_vec @@ Vec3.reflect (1., -1., 0.) (0., 1., 0.);
  (* the second parameter is basically zero (< Float.epsilon_float) *)
  print_vec
  @@ Vec3.reflect (0., -1., 0.) Float.(sqrt 2. /. 2., sqrt 2. /. 2., 0.);
  [%expect {|
    (1., 1., 0.)
    (1., 2.22044604925e-16, 0.) |}]
