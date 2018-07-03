(* FIXME try to generalise? Mostly about weak refs. *)
signature HASH_VAL_REP =
sig
  type 'a t

  val new: int -> 'a t
  val sub: 'a t * int -> 'a option
  val clear: 'a t * int -> unit
  val update: 'a t * int * 'a -> unit
end

signature SYNCHRONIZED =
sig
  type 'a t

  val var: string -> 'a -> 'a t (* FIXME name *)
  val value: 'a t -> 'a
  val change: 'a t -> ('a -> 'a) -> unit
  val change_result: 'a t -> ('a -> 'b * 'a) -> 'b
end

signature THREAD =
sig
  val fork : (unit -> unit) -> unit
end

signature PLATFORM =
sig
  structure Synchronized : SYNCHRONIZED
  structure Thread : THREAD
  structure WeakHashValRep : HASH_VAL_REP

  val exnMessage: exn -> string
  val reraise: exn -> 'a

  val pointer_eq: 'a * 'a -> bool
end
