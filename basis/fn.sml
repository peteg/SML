(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Good description of the infix combinators: http://mlton.org/InfixingOperators *)

(** Utilities for dealing with functions. *)
signature FN = sig
   type ('a, 'b) t = 'a -> 'b
   (** The type of functions. *)

   val const : 'a -> 'b -> 'a
   (** K-combinator ({const x y = x}). *)

   val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
   (** Currying ({curry f x y = f (x, y)}). *)

   val eta : ('a -> 'b -> 'c) -> ('a -> 'b -> 'c)
   (**
    * {eta f x} is equivalent to {case (f, x) of (f, x) => fn y => f x y}.
    * In other words, {eta} delays function application.
    *)

   val fix : (('a -> 'b) -> ('a -> 'b)) -> ('a -> 'b)
   (** Fixpoint of given functional. *)

   val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
   (** Flip the order of arguments ({flip f x y = f y x}). *)

   val id : 'a -> 'a
   (** I-combinator ({id x = x}). *)

   (* val iso : ('a, 'c) Iso.t * ('b, 'd) Iso.t -> (('a, 'b) t, ('c, 'd) t) Iso.t *)
   (** Lifts isos between elements to an iso between arrows. *)

   val map : ('c -> 'a) * ('b -> 'd) -> ('a -> 'b) -> 'c -> 'd
   (** {map (f, g) h = g o h o f}. *)

   val o : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   (** Function composition ({(g o f) x = f (g x)}). *)

   (* val seal : ('a -> 'b) -> 'a -> 'b Thunk.t *)
   (**
    * {seal f x} is equivalent to {fn () => f x} assuming {f} and {x} are
    * variables.
    *)

   val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
   (** Uncurrying ({uncurry f (x, y) = f x y}). *)

   val <\ : 'a * ('a * 'b -> 'c) -> 'b -> 'c
   (** Left section ({(x <\ f) y = f (x, y)}). *)

   val \> : ('a -> 'b) * 'a -> 'b
   (**
    * Left application ({f \> x1 \> ... \> xN = f x1 ... xN}) and infix
    * ({x0 <\f1\> x1 ... <\fN\> xN = fN (... f1 (x0, x1) ..., xN)}).
    *)

   val /> : ('a * 'b -> 'c) * 'b -> 'a -> 'c
   (** Right section ({(f /> y) x = f (x, y)}). *)

   val </ : 'a * ('a -> 'b) -> 'b
   (**
    * Right application ({xN </ ... </ x1 </ f = f x1 ... xN}) and infix
    * ({xN </fN/> ... x1 </f1/> x0 = fN (xN, ... f1 (x1, x0) ...)}).
    *)

   val >| : 'a * ('a -> 'b) -> 'b
   (** Left pipe ({x >| f1 >| ... >| fN = (fN o ... o f1) x}). *)

   val |< : ('a -> 'b) * 'a -> 'b
   (** Right pipe ({fN |< ... |< f1 |< x = (fN o ... o f1) x}). *)
end

structure Fn :> FN =
struct
(*
structure Fn = struct type ('a, 'b) t = 'a -> 'b end
open Fn
*)

type ('a, 'b) t = 'a -> 'b

fun const x _ = x
fun curry f x y = f (x, y)
fun eta f x y = f x y
fun fix f x = f (fix f) x
fun flip f x y = f y x
fun id x = x
fun map (f, g) h = g o h o f
(* fun iso ((a2c, c2a), (b2d, d2b)) = (map (c2a, b2d), map (a2c, d2b)) *)
(* fun seal f x () = f x *)
fun uncurry f (x, y) = f x y
val op o = op o
fun op <\ (x, f) y = f (x, y)
fun op \> (f, y) = f y
fun op /> (f, y) x = f (x, y)
fun op </ (x, f) = f x
val op >| = op </
val op |< = op \>

end

infix  4 <\ \>
infixr 4 </ />
infix  2 >|
infixr 2 |<

val op <\ = op Fn.<\
val op \> = op Fn.\>
val op /> = op Fn./>
val op </ = op Fn.</
val op >| = op Fn.>|
val op |< = op Fn.|<
