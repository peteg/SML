(* Suspensions.

   Rather expensive due to the locking around evaluation.
   Take some care with exceptions ala mlton.
*)

signature Susp =
sig

type 'a t

val delay : (unit -> 'a) -> 'a t
val force : 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t

end

structure Susp :> Susp =
struct

datatype 'a thunk = Val of 'a | Thunk of unit -> 'a | Exn of exn

type 'a t = 'a thunk Platform.Synchronized.t

fun delay (f: unit -> 'a) : 'a t =
  Platform.Synchronized.var "Susp.delay" (Thunk f)

fun force (su : 'a t) : 'a =
  case Platform.Synchronized.value su of (* notionally fast: no lock *)
      Val v => v
    | Exn exn => Platform.reraise exn
    | Thunk _ => (* grab the lock to update the thunk *)
      let
        val update =
         fn Thunk f => ( Val (f ()) handle exn => Exn exn )
          | v => v
      in
        Platform.Synchronized.change su update;
        force su
      end

fun map (f : 'a -> 'b) (su : 'a t) : 'b t =
    delay (fn () => f (force su))

end
