(* Generalized radix tries.

FIXME for dispatching paths in a webserver.

One could imagine using regexps ala flex: yields a more general
language, and after a compile step, a more efficient matcher.

In contrast:
 - we have an infinite alphabet (arbitrary fanout at each node)
   - perhaps but the datatype has finite branching and we expect low fanout.
 - patricia-style tries can compress transitions
 - unlike patricia tries we don't have an upper bound on path length
 - we can futz with the matching semantics (longest match here)
 - no real interest in supporting the Kleene star in general.

FIXME take the longest match. We're here for semantics, not (just) speed.

FIXME add a graft operation?

FIXME GC behaviour, shadowing. AList takes care of trashing
unreachable trees. But what should happen if a later insert shadows an
earlier one? Imagine inserting a long key k1 and then a prefix of it
k2. Do we expect k1 to be reachable still? It will be the longest
match.

See HOL/src/portableML/UTF8Set for a much simpler version.

*)

signature RADIX_KEY =
sig
  type t (* digits *)
  val compare : t Cmp.t

  type key
  val key_empty : key
  (* FIXME longest prefix + leftovers (tail + split). *)
  val key_common_prefix : key * key -> key
  val key_tails : key * key -> (key * t * key) option * (key * t * key) option
end

signature RADIX_TRIE =
sig
  type key
  type 'a t

  val empty : 'a t
  val lookup: 'a t -> key -> ('a * key) option
  val insert : ('a * 'a -> 'a) -> key * 'a -> 'a t -> 'a t
end

functor Radix_trie(Key: RADIX_KEY) :> RADIX_TRIE where type key = Key.key =
struct

type key = Key.key
structure BrMap = AList(Key)

datatype 'a t
  = Empty
  | Leaf of Key.key * 'a
  | Branch of Key.key * 'a option * 'a t BrMap.t

val empty = Empty

(* FIXME is tail recursion better here? Then we have to build matches on the way down... *)
fun lookup_longest_match (k : key) : 'a t -> ('a * key) option =
  fn Empty => NONE
  | Leaf (lk, lv) =>
    ( case Key.key_tails (lk, k) of
          (NONE, NONE) => SOME (lv, Key.key_empty)
        | (NONE, SOME (k', _, _)) => SOME (lv, k')
        | _ => NONE )
  | Branch (bk, bv, bcs) =>
    ( case Key.key_tails (bk, k) of
          (NONE, NONE) => Option.map (fn v => (v, Key.key_empty)) bv
        | (NONE, SOME (k', d, suff)) =>
          let open OptM
          in case BrMap.lookup bcs d >>= (fn t' =>
                                             lookup_longest_match suff t') of
                 NONE => Option.map (fn v => (v, k')) bv
               | (ak as SOME _) => ak
          end
        | _ => NONE )

fun lookup t k = lookup_longest_match k t

fun insert (c: 'a * 'a -> 'a) ((k, v): key * 'a) (t: 'a t) : 'a t =
    let
      fun ins k v t : 'a t =
          case t of
              Empty => Leaf (k, v)
            | Leaf (lk, lv) =>
              ( case Key.key_tails (lk, k) of
                    (NONE, NONE) => Leaf (lk, c (v, lv))
                  | (NONE, SOME (_, d, suff)) => Branch (lk, SOME lv, ins_BrMap d suff v  BrMap.empty)
                  | (SOME (_, d, suff), NONE) => Branch (k,  SOME v,  ins_BrMap d suff lv BrMap.empty)
                  | (SOME (_, d0, suff0), SOME (_, d1, suff1)) =>
                    Branch ( Key.key_common_prefix (lk, k)
                           , NONE
                           , ins_BrMap d0 suff0 lv (ins_BrMap d1 suff1 v BrMap.empty) ) )
            | Branch (bk, bv, bcs) =>
              ( case Key.key_tails (bk, k) of
                    (NONE, NONE) =>
                    Branch (bk, SOME (case bv of NONE => v | SOME v' => (c (v, v'))), bcs)
                  | (NONE, SOME (_, d, suff)) =>
                    Branch (bk, bv, ins_BrMap d suff v bcs)
                  | (SOME (_, d, suff), NONE) =>
                    Branch (k, SOME v, BrMap.from_list [ (d, Branch (suff, bv, bcs))])
                  | (SOME (_, d0, suff0), SOME (_, d1, suff1)) =>
                    Branch ( Key.key_common_prefix (bk, k)
                           , NONE
                           , ins_BrMap' d0 (Branch (suff0, bv, bcs)) (ins_BrMap d1 suff1 v BrMap.empty) ) )
      and ins_BrMap (d : Key.t) (suff : Key.key) (v : 'a) (cs : 'a t BrMap.t) : 'a t BrMap.t =
          BrMap.insert (fn t' => case t' of
                                   NONE => Leaf (suff, v)
                                 | SOME t' => ins suff v t') d cs
      and ins_BrMap' (d : Key.t) (t': 'a t) (cs: 'a t BrMap.t) : 'a t BrMap.t =
          BrMap.insert (fn t'' => case t'' of
                                    NONE => t'
                                  | SOME _ => raise FIXME "ins_BrMap' impossible, I hope") d cs
    in
      ins k v t
    end

end

(* FIXME generalize to type key = 'a list. Split signature above? *)
structure Radix_key_string =
struct
  type t = string
  val compare = String.compare

  type key = string list

  val key_empty = []
  fun key_split (k : key) : (t * key) option =
      case k of
          [] => NONE
        | d :: ds => SOME (d, ds)

  fun k1k2 (k1, k2) =
      "(" ^ String.concatWith "," k1 ^ "; " ^ String.concatWith "," k2 ^ ")"

  fun key_common_prefix (ks : key * key) : key =
    let
      (* val () = print ("StringListTrie.key_common_prefix: " ^ k1k2 ks) *)
      fun aux pref =
        fn ([], _) => List.rev pref
        | (_, []) => List.rev pref
        | (d1 :: k1, d2 :: k2) =>
          case compare (d1, d2) of
              EQUAL => aux (d1 :: pref) (k1, k2)
            | _ => List.rev pref
    in
      aux [] ks
    end

  fun list_to_option xxs =
    case xxs of
        [] => NONE
      | x :: xs => SOME (xxs, x, xs)

  fun key_tails (ks : key * key) : (key * t * key) option * (key * t * key) option =
    let
      (* val () = print ("StringListTrie.match_prefix: " ^ k1k2 ks ^ "\n") *)
      val rec aux =
       fn ([], r) => (NONE, list_to_option r)
        | (l, []) => (list_to_option l, NONE)
        | (l as d1 :: k1, r as d2 :: k2) =>
          case compare (d1, d2) of
              EQUAL => aux (k1, k2)
            | _ => (list_to_option l, list_to_option r)
    in
      aux ks
    end
end

structure StringListTrie = Radix_trie(Radix_key_string)

(* FIXME testing *)
(*
open StringListTrie
val ins = insert #1

val t : int map = ins (["1"], 1) empty
val u : int map = ins (["1", "2"], 2) t
val v : int map = ins (["1", "3"], 3) u
val w : int map = ins (["0", "0"], 0) v
val x : int map = ins ([], ~1) w
*)
