(* A FIFO buffer, intended as a communication pipe.

Check what happens if we get interrupted / async exns.

Intended to be single-receiver. Could be further optimized for
that. Plays into the correctness of the timeout parameter to get: we
wait until there's at least one thing in the queue, and no other
thread will remove it.

FIXME really want the receiver process to be GCd if the sender thread
goes away.

*)

signature FIFO_CHANNEL =
sig
  type 'a t
  val create : unit -> 'a t
  val get : 'a t * Time.time option -> 'a option
  val put : 'a t * 'a -> unit
end;

structure Fifo_channel :> FIFO_CHANNEL =
struct

datatype 'a t = Q of 'a Fifo.t Platform.Synchronized.t

val name = "Fifo_channel"

fun create () =
  Q (Platform.Synchronized.var name (Fifo.empty))

fun get (Q q, time_limit) =
  let
    fun update q =
      case Fifo.next q of
          NONE => (NONE, q)
        | SOME (a, q') => (SOME a, q')
  in
(*    Platform.Synchronized.timed_access q (K time_limit) update *)
    Platform.Synchronized.change_result q update
  end

fun put (Q q, x) =
  Platform.Synchronized.change q (fn q => Fifo.enqueue (q, x))

end
