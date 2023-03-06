open! Core

type t = float * float * float

(* creates a new vector with the given component values *)
let create x y z : t = (x, y, z)

(* Creates a new vector with each component uniformly random between min/max
   (max is exclusive, like Float.random) *)
let random ?(min = 0.) max : t =
  let diff = max -. min in
  (min +. Random.float diff, min +. Random.float diff, min +. Random.float diff)

(* computes the length of a vector *)
let length (x, y, z) = Float.(sqrt (square x + square y + square z))
let length_squared vec = Float.square @@ length vec

(* adds two vectors *)
let add (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)
let add_scal (x, y, z) scalar = (x +. scalar, y +. scalar, z +. scalar)

(* subtracts two vectors *)
let sub (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)

(* negates a tuple *)
let neg (x, y, z) = (-.x, -.y, -.z)

(* scales a vector by a float factor *)
let scale ((x, y, z) : t) by = (x *. by, y *. by, z *. by)

(* as scale, but divides by the scale factor *)
let scale_div ((x, y, z) : t) by = (x /. by, y /. by, z /. by)
let sqrt_exn ((x, y, z) : t) = (Float.sqrt x, Float.sqrt y, Float.sqrt z)

(* scales the vector to have length 1 *)
let unit vec =
  let length = length vec in
  scale_div vec length

(* compute the dot product of two vectors *)
let dot (x1, y1, z1) (x2, y2, z2) : float =
  (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

(* compute the cross product of two vectors. *)
let cross (x1, y1, z1) (x2, y2, z2) =
  ((y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2))

(* multiplies two vectors by multiplying each component individually
   (also known as the Hadamard product or Schur product)
*)
let pairwise_mult (x1, y1, z1) (x2, y2, z2) = (x1 *. x2, y1 *. y2, z1 *. z2)

(* Returns true if the vector is very close to zero in all dimensions.
   Useful to disallow certain vectors in teh case where math would create
   infinities *)
let near_zero (x, y, z) =
  let nz_epsilon = Float.epsilon_float *. 2. in
  Float.(abs x < nz_epsilon)
  && Float.(abs y < nz_epsilon)
  && Float.(abs z < nz_epsilon)

(* Given a vector, returns the reflection of that vector against a surface
   with the provided normal *)
let reflect (vector : t) (normal : t) : t =
  sub vector (scale normal (2. *. dot vector normal))

(* converts a vector into a string *)
let to_s (a, b, c) = Printf.sprintf "(%F, %F, %F)" a b c

(* Infix Operators

   Note: +/- infix operators are straightforward,
   since there are multiple ways of multiplying
   vectors I kept those as functions.
   Avoid overloading the float operators *., /., etc.
   since then if Vec3 is opened it would override those
   (even in this file I'd then have to
   explicitly open float to do any kind of float math)
*)

let ( + ) = add
let ( - ) = sub
