(*

FIXME probably assumes 32 bit everything.
FIXME salt `combine` hash
*)

structure Hashing =
struct

(* Good sizes for hashtables according to: http://planetmath.org/goodhashtableprimes *)
val primes : int vector =
    Vector.fromList
      [ 53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613
      , 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653
      , 100663319, 201326611, 402653189, 805306457, 1610612741 ]

fun nextSize (i: int) : int =
    Option.valOf (Vector.find (fn x => i < x) primes) (* FIXME *)

(* Combine two hashes. *)
fun combine (x: int, y: int) : int =
    Word.toInt (Word.xorb (Word.* (Word.fromInt x, 0w16777619), Word.fromInt y))

end
