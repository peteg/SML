(*  Simple association lists. Ordered, therefore opaque. Uses Isabelle's KEY signature. *)
(* FIXME why not satisfy the TABLE signature? *)

signature ALIST =
sig
  type key
  type 'v t
  val compare : 'v Cmp.t -> 'v t Cmp.t
  val empty : 'v t
  val insert : ('v option -> 'v) -> key -> 'v t -> 'v t (* FIXME clunky.*)
  val length : 'v t -> int
  val lookup : 'v t -> key -> 'v option
  val lookup_index : 'v t -> key -> int option
  val app : (key * 'v -> unit) -> 'v t -> unit
  exception UnequalKeys
  val appEq : ('v * 'w -> unit) -> ('v t * 'w t) -> unit (* raises UnequalKeys, ListPair.UnequalLengths *)
  val exists : (key * 'v -> bool) -> 'v t -> bool
  val exists_val : ('v -> bool) -> 'v t -> bool
  val foldl : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val map : (key * 'v -> 'w) -> 'v t -> 'w t
  val mapv : ('v -> 'w) -> 'v t -> 'w t
  val from_list : (key * 'v) list -> 'v t
  val to_list : 'v t -> (key * 'v) list
end

functor AList(Key: KEY) :> ALIST where type key = Key.t =
struct

type key = Key.t
type 'v t = (key * 'v) list

fun compare vcmp = List.compare (Pair.compare (Key.compare, vcmp))

val empty : (key * 'v) list = []

fun insert c k =
 fn [] => [(k, c NONE)]
  | alist as ((kv' as (k', v')) :: alist') =>
    ( case Key.compare (k, k') of
          LESS => (k, c NONE) :: alist
        | EQUAL => (k', c (SOME v')) :: alist'
        | GREATER => kv' :: insert c k alist' )

fun lookup alist k =
    case alist of
        [] => NONE
      | (k', v') :: alist' =>
        ( case Key.compare (k, k') of
              LESS => NONE
            | EQUAL => SOME v'
            | GREATER => lookup alist' k )

fun lookup_index alist k =
    let
      fun aux i =
        fn [] => NONE
        | (k', _) :: alist' =>
          ( case Key.compare (k, k') of
                LESS => NONE
              | EQUAL => SOME i
              | GREATER => aux (i+1) alist' )
    in
      aux 0 alist
    end

val length = List.length

fun app f =
  fn [] => ()
  | kv :: kvs => (f kv; app f kvs)

exception UnequalKeys

fun appEq f =
 fn ((kx, vx) :: xs, (ky, vy) :: ys) =>
    if Key.compare (kx, ky) = EQUAL
    then ( f (vx, vy); appEq f (xs, ys) )
    else raise UnequalKeys
  | ([], []) => ()
  | _ => raise ListPair.UnequalLengths

fun exists f =
 fn [] => false
 | kv :: kvs => f kv orelse exists f kvs

fun exists_val f =
 fn [] => false
 | (_, v) :: kvs => f v orelse exists_val f kvs

fun foldl f b =
 fn [] => b
  | (k, v) :: kvs => foldl f (f (k, v, b)) kvs

fun map f =
 fn [] => []
  | (k, v) :: alist => (k, f (k, v)) :: map f alist

fun mapv f =
 fn [] => []
  | (k, v) :: alist => (k, f v) :: mapv f alist

(* Retain the first of any duplicates. This is what an AList.lookup would find. *)
fun from_list (xs: (key * 'v) list) : 'v t =
  Library.sort_distinct (Key.compare o Pair.bimap (#1, #1)) xs

fun to_list xs = xs

end

structure IntAList = AList(Int)
structure StringAList = AList(String)
