open! Core

type t = float * float * float

(* creates a new vector with the given component values *)
let create x y z : t = (x, y, z)

(* computes the length of a vector *)
let length (x, y, z) = Float.(sqrt (square x + square y + square z))

(* adds two vectors *)
let add (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)

(* subtracts two vectors *)
let sub (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)

(* negates a tuple *)
let neg (x, y, z) = (-.x, -.y, -.z)

(* scales a vector by a float factor *)
let scale ((x, y, z) : t) by = (x *. by, y *. by, z *. by)

(* as scale, but divides by the scale factor *)
let scale_div ((x, y, z) : t) by = (x /. by, y /. by, z /. by)

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
