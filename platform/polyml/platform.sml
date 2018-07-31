use "../platform.sig";
use "poly_threads.sml";
use "poly_smlnj-lib.sml";

structure Platform :> PLATFORM =
struct

structure Synchronized : SYNCHRONIZED = Synchronized

(* FIXME probably want a thread pool + discard excess connections. Green threads too. *)
(* FIXME be a little friendly and report exns that leak. *)
structure Thread : THREAD =
struct

fun fork f = ignore (Thread.Thread.fork (f, [])) (* FIXME attributes *)

end

structure WeakHashValRep :> HASH_VAL_REP =
struct

type 'a t = 'a ref option array

fun new (size: int): 'a t = Weak.weakArray (size, NONE)
fun sub (t: 'a t, i: int): 'a option = Option.map ! (Array.sub (t, i))
fun clear (t: 'a t, i: int) = Array.update (t, i, NONE)
fun update (t: 'a t, i: int, v: 'a) = Array.update (t, i, SOME (ref v))

end

val exnMessage = General.exnMessage
val reraise = PolyML.Exception.reraise

val pointer_eq = PolyML.pointerEq

end
