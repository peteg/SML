structure Platform :> PLATFORM =
struct

(* FIXME synchronous. *)
structure Synchronized =
struct

type 'a t = 'a ref

fun var _ v = ref v
fun value r = !r
fun change r f = r := f (!r)
fun change_result r f = let val (a, v) = f (!r) in r := v; a end

end

(* FIXME synchronous. Perhaps try out green threads or similar. *)
structure Thread :> THREAD =
struct

fun fork f = f ()

end

(* FIXME this only works for heap-allocated types. http://mlton.org/MLtonWeak *)
(* FIXME guess: if the 'a is not referred to elsewhere, can nuke it. *)
structure WeakHashValRep :> HASH_VAL_REP =
struct

type 'a t = 'a MLton.Weak.t option array

fun new (size: int): 'a t = Array.array (size, NONE)
fun sub (t: 'a t, i: int): 'a option =
    Option.mapPartial MLton.Weak.get (Array.sub (t, i))
fun clear (t: 'a t, i: int) : unit = Array.update (t, i, NONE)
fun update (t: 'a t, i: int, v: 'a) : unit =
    Array.update (t, i, SOME (MLton.Weak.new v))

end

fun exnMessage exn =
    String.concatWith "\n" (General.exnMessage exn :: MLton.Exn.history exn)
fun reraise exn = raise exn (* FIXME preserve stack backtrace *)

val pointer_eq = MLton.eq

end
