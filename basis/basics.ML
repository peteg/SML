(*
ISABELLE COPYRIGHT NOTICE, LICENCE AND DISCLAIMER.

Copyright (c) 1986-2014,
  University of Cambridge,
  Technische Universitaet Muenchen,
  and contributors.

  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

* Neither the name of the University of Cambridge or the Technische
Universitaet Muenchen nor the names of their contributors may be used
to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
(*  Title:      Pure/General/basics.ML
    Author:     Florian Haftmann and Makarius, TU Muenchen

Fundamental concepts.
*)

infix 1 |> |-> |>> ||> ||>>
infix 1 #> #-> #>> ##> ##>>

signature BASICS =
sig
  (*functions*)
  val |> : 'a * ('a -> 'b) -> 'b
  val |-> : ('c * 'a) * ('c -> 'a -> 'b) -> 'b
  val |>> : ('a * 'c) * ('a -> 'b) -> 'b * 'c
  val ||> : ('c * 'a) * ('a -> 'b) -> 'c * 'b
  val ||>> : ('c * 'a) * ('a -> 'd * 'b) -> ('c * 'd) * 'b
  val #> : ('a -> 'b) * ('b -> 'c) -> 'a -> 'c
  val #-> : ('a -> 'c * 'b) * ('c -> 'b -> 'd) -> 'a -> 'd
  val #>> : ('a -> 'c * 'b) * ('c -> 'd) -> 'a -> 'd * 'b
  val ##> : ('a -> 'c * 'b) * ('b -> 'd) -> 'a -> 'c * 'd
  val ##>> : ('a -> 'c * 'b) * ('b -> 'e * 'd) -> 'a -> ('c * 'e) * 'd
  val ` : ('b -> 'a) -> 'b -> 'a * 'b
  val tap: ('b -> 'a) -> 'b -> 'b

  (*options*)
  val is_some: 'a option -> bool
  val is_none: 'a option -> bool
  val the: 'a option -> 'a
  val these: 'a list option -> 'a list
  val the_list: 'a option -> 'a list
  val the_default: 'a -> 'a option -> 'a
  val perhaps: ('a -> 'a option) -> 'a -> 'a
  val merge_options: 'a option * 'a option -> 'a option

  (*partiality*)
  val try: ('a -> 'b) -> 'a -> 'b option
  val can: ('a -> 'b) -> 'a -> bool

  (*lists*)
  val cons: 'a -> 'a list -> 'a list
  val append: 'a list -> 'a list -> 'a list
  val fold: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val fold_rev: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val fold_map: ('a -> 'b -> 'c * 'b) -> 'a list -> 'b -> 'c list * 'b
end;

structure Basics: BASICS =
struct

(* functions *)

(*application and structured results*)
fun x |> f = f x;
fun (x, y) |-> f = f x y;
fun (x, y) |>> f = (f x, y);
fun (x, y) ||> f = (x, f y);
fun (x, y) ||>> f = let val (z, y') = f y in ((x, z), y') end;

(*composition and structured results*)
fun (f #> g) x   = x |> f |> g;
fun (f #-> g) x  = x |> f |-> g;
fun (f #>> g) x  = x |> f |>> g;
fun (f ##> g) x  = x |> f ||> g;
fun (f ##>> g) x = x |> f ||>> g;

(*result views*)
fun `f = fn x => (f x, x);
fun tap f = fn x => (f x; x);


(* options *)

fun is_some (SOME _) = true
  | is_some NONE = false;

fun is_none (SOME _) = false
  | is_none NONE = true;

fun the (SOME x) = x
  | the NONE = raise Option.Option;

fun these (SOME x) = x
  | these NONE = [];

fun the_list (SOME x) = [x]
  | the_list NONE = []

fun the_default _ (SOME y) = y
  | the_default x NONE = x;

fun perhaps f x = the_default x (f x);

fun merge_options (x, y) = if is_some x then x else y;


(* partiality *)

fun try f x = SOME (f x)
  handle exn => (* if Exn.is_interrupt exn then reraise exn else *) NONE;

fun can f x = is_some (try f x);


(* lists *)

fun cons x xs = x :: xs;

fun append xs ys = xs @ ys;

fun fold _ [] y = y
  | fold f (x :: xs) y = fold f xs (f x y);

fun fold_rev _ [] y = y
  | fold_rev f (x :: xs) y = f x (fold_rev f xs y);

fun fold_map _ [] y = ([], y)
  | fold_map f (x :: xs) y =
      let
        val (x', y') = f x y;
        val (xs', y'') = fold_map f xs y';
      in (x' :: xs', y'') end;

end;

(* open Basics; *)
