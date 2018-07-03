(*

FIXME describe
Linear probing
Ephemeral
Weakly-referenced values
Hashtable

Some ideas from Arlen Cox's presentation in ocaml:
https://github.com/arlencox/mlbdd

and David Matthews's `HashArray` in the Poly/ML distribution.

FIXME not using the hash-cons lib in smlnj-lib as it does not use weak
pointers.

FIXME only grows, never shrinks

FIXME Poly/ML isms:
 - Weak module (forces the `'a ref option array` representation)
 - pull out a platform-specific module

FIXME note compatibility conditions on HASHED_TYPE

FIXME Do we need to use Weak.touch anywhere?

FIXME hangs onto keys longer than one might like because we don't have
finalizers.

*)

signature HASH_KEY =
sig
  type t
  val equal: t * t -> bool
  val hash: t -> int
end

structure BoolHashKey : HASH_KEY =
struct

type t = bool

val equal = op =
fun hash (x: bool) : int = if x then 1 else 0

end

structure IntHashKey : HASH_KEY =
struct

type t = int

val equal = op =
fun hash (x: int) : int =
    x (* FIXME Haskell's hashable suggests using this, but the real action might be in the hashtable, not hashable *)

end

functor PairHashKey(structure K1: HASH_KEY
                    structure K2: HASH_KEY) : HASH_KEY =
struct

type t = K1.t * K2.t

fun equal ((k1, k2), (k1', k2')) =
    K1.equal (k1, k1') andalso K2.equal (k2, k2')
fun hash (k1, k2) = Hashing.combine (K1.hash k1, K2.hash k2)

end

functor TripleHashKey(structure K1: HASH_KEY
                      structure K2: HASH_KEY
                      structure K3: HASH_KEY) : HASH_KEY =
struct

type t = K1.t * K2.t * K3.t

fun equal ((k1, k2, k3), (k1', k2', k3')) =
    K1.equal (k1, k1') andalso K2.equal (k2, k2') andalso K3.equal (k3, k3')
fun hash (k1, k2, k3) =
    Hashing.combine (Hashing.combine (K1.hash k1, K2.hash k2), K3.hash k3)

end

structure HashSet : HASH_VAL_REP =
struct

type 'a t = unit

fun new (size: int): 'a t = ()
fun sub (t: 'a t, i: int): 'a option = NONE
fun clear (t: 'a t, i: int) = ()
fun update (t: 'a t, i: int, v: 'a) = ()

end

structure FIXMEStandard : HASH_VAL_REP =
struct

type 'a t = 'a option array

fun new (size: int): 'a t = Array.array (size, NONE)
fun sub (t: 'a t, i: int): 'a option = Array.sub (t, i)
fun clear (t: 'a t, i: int) = Array.update (t, i, NONE)
fun update (t: 'a t, i: int, v: 'a) = Array.update (t, i, SOME v)

end

signature HASH_TABLE =
sig
  type 'a t

  structure Key : HASH_KEY

  val new: int -> 'a t
  val length: 'a t -> int

  val update: 'a t * Key.t * 'a -> unit
  val sub: 'a t * Key.t -> 'a option
(*  val appi: (Key.t * 'a -> unit) -> 'a t -> unit FIXME maybe later *)
  val exists: 'a t * Key.t -> bool
  val delete: 'a t * Key.t -> unit
end

functor HashTable(structure K: HASH_KEY structure V: HASH_VAL_REP)
   :> HASH_TABLE where type Key.t = K.t =
struct

structure Key = K

type hk = int * Key.t
datatype key
  = Entry of hk
  | Tombstone
  | Empty

type 'a t =
     { keys: key array ref
     , vals: 'a V.t ref
     , length: int ref
     }

fun hit ((h, k): hk, (h', k'): hk) : bool =
    h = h' andalso Key.equal (k, k')

fun new (size: int) : 'a t =
    let
      val size = Hashing.nextSize size
    in
      { keys = ref (Array.array (size, Empty))
      , vals = ref (V.new size)
      , length = ref 0
      }
    end

fun length ({length, ...}: 'a t) : int = !length

fun update_raw (ht as {keys = ref keys, vals = ref vals, length}: 'a t, hk: hk, v: 'a) : unit =
    let
      val len = Array.length keys
      fun go i =
          let
            val i = i mod len
          in
            case Array.sub (keys, i) of
                Entry hk' =>
                if hit (hk, hk')
                then (V.update (vals, i, v); false)
                else go (i + 1)
            | Tombstone =>
              let (* Can insert here, but need to remove any later `Entry` for hk *)
                fun rmEntry i =
                    let
                      val i = i mod len
                    in
                      case Array.sub (keys, i) of
                          Entry hk' =>
                          if hit (hk, hk')
                          then Array.update (keys, i, Tombstone)
                          else rmEntry (i + 1)
                        | Tombstone => rmEntry (i + 1)
                        | Empty => ()
                    end
              in
                rmEntry (i + 1)
              ; Array.update (keys, i, Entry hk)
              ; V.update (vals, i, v)
              ; false
              end
            | Empty =>
              let in
                Array.update (keys, i, Entry hk)
              ; V.update (vals, i, v)
              ; true
              end
          end
    in
      if go (#1 hk mod len)
      then length := !length + 1
      else ()
    end

fun resize (ht as {keys, vals, length}: 'a t) : unit =
    let
      val ht' = new (Hashing.nextSize (Array.length (!keys)))
      fun go (i, Entry hk) =
          (case V.sub (!vals, i) of
               NONE => ()
             | SOME v => update_raw (ht, hk, v))
        | go _ = ()
    in
      Array.appi go (!keys)
    ; keys := !(#keys ht')
    ; vals := !(#vals ht')
    ; length := !(#length ht')
    end

fun update (ht as {keys = ref keys, length, ...}: 'a t, k: Key.t, v: 'a) : unit =
    let in
      if 2 * !length + 1 > Array.length keys
      then resize ht
      else ()
    ; update_raw (ht, (Key.hash k, k), v)
    end

fun sub_raw (ht as {keys = ref keys, vals = ref vals, ...}: 'a t, k: Key.t) : ('a * int) option =
    let
      val hk = (Key.hash k, k)
      val len = Array.length keys
      fun go i =
          let
            val i = i mod len
          in
            case Array.sub (keys, i) of
                Entry hk' =>
                if hit (hk, hk')
                then case V.sub (vals, i) of
                         (* garbage collected; remove key too *)
                         NONE => (Array.update (keys, i, Tombstone); NONE)
                       | SOME v => SOME (v, i)
                else go (i + 1)
              | Tombstone => go (i + 1)
              | Empty => NONE
          end
    in
      go (#1 hk)
    end

fun sub (ht: 'a t, k: Key.t) : 'a option =
    Option.map #1 (sub_raw (ht, k))

fun exists (ht: 'a t, k: Key.t) : bool =
    Option.isSome (sub (ht, k))

fun delete (ht as {keys = ref keys, vals = ref vals, ...}: 'a t, k: Key.t) : unit =
    case sub_raw (ht, k) of
        NONE => ()
      | SOME (_, i) =>
        let in
          Array.update (keys, i, Tombstone)
        ; V.clear (vals, i)
        end

end
