(*

Boolean Decision Diagrams.

Based on Arlen Cox's presentation in ocaml:
https://github.com/arlencox/mlbdd

FIXME note complement structure.

FIXME contemplate parallelisation
 - lock-free work stealing?
 - make the hashtable concurrency safe somehow? -- maybe buckets are better than probing

FIXME rename edge0, edge1 -> hi, lo ?

*)

(* FIXME implementation/kernel *)
signature BDD_IMPL =
sig
  type t
  type var = int
  type man

  val new: int -> man

  val compare : t * t -> General.order

  val ff: t
  val tt: t
  val var: man * var -> t

  val not_: t -> t
  val and_: man * t * t -> t
  val or: man * t * t -> t
  val nand: man * t * t -> t
  val implies: man * t * t -> t
  val xor: man * t * t -> t
  val eq: man * t * t -> t
  val nxor: man * t * t -> t

  type support
  val support: man * t -> support

  val exists: man * support * t -> t

  datatype 'a view
    = False
    | True
    | If of {var: var, edge0: 'a, edge1: 'a}

  val fold: man * ('a view * 'b -> 'a * 'b) * 'b * t -> 'a * 'b
  val view: t -> t view
end

structure Bdd_impl :> BDD_IMPL =
struct

type code = int
type var = int

datatype node
  = NFalse
  | NIf of ite
withtype ite
  = { var: var
    , edge0: node
    , edge1: {node: node, polarity: bool}
    , code: code (* always even to allow room for complements FIXME proxy for address/unique node id *)
    }

type t
  = { node: node
    , polarity: bool
    }

val code : node -> code =
 fn NFalse => 0
  | NIf r => #code r

fun tcode (x: t) : code =
    code (#node x) + (if #polarity x then 1 else 0)

fun compare (x: t, y: t): General.order =
 Int.compare (tcode x, tcode y)

val ff : t = {node = NFalse, polarity = false}
val tt : t = {node = NFalse, polarity = true}
fun not_ ({node, polarity}: t) : t  = {node = node, polarity = not polarity}

fun is_ff (x: t) : bool = #node x = NFalse andalso not (#polarity x)
fun is_tt (x: t) : bool = #node x = NFalse andalso #polarity x

(* Normalise an `ite`: one step in a traversal. *)
fun norm (ite: ite, polarity: bool) : t * t =
  ( {node = #edge0 ite, polarity = polarity}
  , if polarity then not_ (#edge1 ite) else #edge1 ite )

(* FIXME consider using PairHashKey or defining TripleHashKey *)
structure IfHashCons =
  HashTable(
    structure K = TripleHashKey(structure K1 = IntHashKey
                                structure K2 = IntHashKey
                                structure K3 = IntHashKey)
    structure V = Platform.WeakHashValRep)

structure AndHashCons =
  HashTable(
    structure K = PairHashKey(structure K1 = IntHashKey
                              structure K2 = IntHashKey)
    structure V = Platform.WeakHashValRep)

type man
     = { if_hc : node IfHashCons.t
       , if_hc_code : int ref
       , and_hc : t AndHashCons.t
       }
(* FIXME
    xor_cache : (int * int,t) Hashtbl.t;
*)

fun new (size: int) : man =
    { if_hc = IfHashCons.new size
    , if_hc_code = ref 2
    , and_hc = AndHashCons.new size
    }

fun cif (man: man, var: var, t0: t, t1: t) : t =
    let
      (* Normalize FIXME diagram *)
      val polarity = #polarity t0
      val t0 = #node t0
      val t1 = if polarity then not_ t1 else t1

      val code0 = code t0
      val code1 = tcode t1
    in
      if code0 = code1
      then {node = t0, polarity = polarity}
      else
        let
          val key = (var, code0, code1)
        in
          case IfHashCons.sub (#if_hc man, key) of
              SOME node => {node = node, polarity = polarity}
            | NONE =>
              let
                (* FIXME if, not create a new id (always even!) *)
                val code = !(#if_hc_code man)
                val () = #if_hc_code man := code + 2
                val node = NIf {var = var, edge0 = t0, edge1 = t1, code = code}
                val () = IfHashCons.update (#if_hc man, key, node)
              in
                {node = node, polarity = polarity}
              end
        end
    end

fun var (man: man, v: var) : t =
    cif (man, v, tt, ff)

(* FIXME needs more scrute *)
fun and_ ( man: man
         , e0 as {node = node0, polarity = polarity0}: t
         , e1 as {node = node1, polarity = polarity1}: t) : t =
    case (node0, node1) of
        (NFalse, _) => if polarity0 then e1 else ff
      | (_, NFalse) => if polarity1 then e0 else ff
      | (NIf r0, NIf r1) =>
        let
          val code0 = tcode e0
          val code1 = tcode e1
        in
          if code0 = code1
          then e0
          else
            let
              val key = if code0 < code1 then (code0, code1) else (code1, code0)
            in
              case AndHashCons.sub (#and_hc man, key) of
                  SOME t => t
                | NONE =>
                  let
                    val (e00, e01) = norm (r0, polarity0)
                    val (e10, e11) = norm (r1, polarity1)
                    val t =
                        case Int.compare (#var r0, #var r1) of
                            LESS    => cif (man, #var r0, and_ (man, e00, e1),  and_ (man, e01, e1))
                          | EQUAL   => cif (man, #var r0, and_ (man, e00, e10), and_ (man, e01, e11))
                          | GREATER => cif (man, #var r1, and_ (man, e10, e0),  and_ (man, e11, e0))
                  in
                    AndHashCons.update (#and_hc man, key, t)
                  ; t
                  end
            end
        end

fun or (man, e0, e1) = not_ (and_ (man, not_ e0, not_ e1))
fun nand (man, e0, e1) = not_ (and_ (man, e0, e1))
fun implies (man, e0, e1) = or (man, not_ e0, e1)
fun xor (man, e0, e1) = and_ (man, or (man, e0, e1), nand (man, e0, e1)) (* FIXME sufficiently horrible to justify a special treatment of xor? *)
fun eq (man, e0, e1) = not_ (xor (man, e0, e1))
val nxor = eq (* FIXME xnor? *)

(* Traversals require tracking visited nodes. FIXME *)
structure Visited =
  HashTable(structure K = IntHashKey
            structure V = FIXMEStandard)

structure CVisited =
  HashTable(structure K = PairHashKey(structure K1 = IntHashKey
                                      structure K2 = BoolHashKey)
            structure V = FIXMEStandard)

(* FIXME inserts are inefficient in BDD order; otherwise pretty good. Consider reversing the order? *)
structure Support = IntListSet
type support = Support.set

fun support (man: man, {node, ...}: t) : support =
    let
      (* FIXME visited should be a set here. *)
      val visited = Visited.new ((IfHashCons.length (#if_hc man) * 3) div 2) (* FIXME load factor *)
      fun go (support: support, n: node) : support =
          case n of
              NFalse => support
            | NIf r =>
              if Visited.exists (visited, #code r)
              then support
              else
                let in
                  Visited.update (visited, #code r, ())
                ; go (go (Support.add (support, #var r), #edge0 r), #node (#edge1 r))
                end
    in
      go (Support.empty, node)
    end

fun exists (man: man, support: support, t: t) : t =
    let
      (* FIXME do we really need to take care with the complement arcs? *)
      val visited = CVisited.new ((IfHashCons.length (#if_hc man) * 3) div 2) (* FIXME load factor *)
      fun go (vvs: var list, (t as {node, polarity}: t)) : t =
          case (vvs, node) of
              (_, NFalse) => t
            | ([], _) => t
            | (v :: vs, NIf r) =>
              case CVisited.sub (visited, (#code r, polarity)) of
                  SOME t => t
                | NONE =>
                  let
                    val t =
                        case Int.compare (v, #var r) of
                            LESS    => go (vs, t)
                          | EQUAL   =>
                            let
                              val (t0, t1) = norm (r, polarity)
                            in
                              or (man, go (vs, t0), go (vs, t1))
                            end
                          | GREATER =>
                            let
                              val (t0, t1) = norm (r, polarity)
                            in
                              cif (man, #var r, go (vvs, t0), go (vvs, t1))
                            end
                  in
                    CVisited.update (visited, (#code r, polarity), t)
                  ; t
                end
    in
      go (Support.toList support, t)
    end

datatype 'a view
  = False
  | True
  | If of {var: var, edge0: 'a, edge1: 'a}

(* FIXME use this to write `support` and others? *)
(* FIXME don't need man except to size the initial hashtable *)
fun fold (man: man, f: 'a view * 'b -> 'a * 'b, s: 'b, t: t) : 'a * 'b =
    let
      val visited = Visited.new ((IfHashCons.length (#if_hc man) * 3) div 2) (* FIXME load factor *)
      fun go (s: 'b, t: t) : 'a * 'b =
          case Visited.sub (visited, tcode t) of
              SOME a => (a, s)
            | NONE =>
              let
                val (a, s) : 'a * 'b =
                    case #node t of
                        NFalse => f (if #polarity t then True else False, s)
                      | NIf r =>
                        let
                          val (e0, e1) = norm (r, #polarity t)
                          val (e0, s) = go (s, e0)
                          val (e1, s) = go (s, e1)
                        in
                          f (If {var = #var r, edge0 = e0, edge1 = e1}, s)
                        end
              in
                Visited.update (visited, tcode t, a)
              ; (a, s)
              end
    in
      go (s, t)
    end

fun view (t: t) : t view =
    case #node t of
        NFalse => (if #polarity t then True else False)
      | NIf r =>
        let
          val (e0, e1) = norm (r, #polarity t)
        in
          If {var = #var r, edge0 = e0, edge1 = e1}
        end

end

structure Bdd_ext =
struct

local
  structure Bdd = Bdd_impl
in

fun toDot (man: Bdd.man, t: Bdd.t) : Layout.t =
    let
      val graphOptions = [Dot.GraphOption.RankDir Dot.TopToBottom]
      val leafOptions = [Dot.NodeOption.Shape Dot.Box]
      fun varOptions var = [ Dot.NodeOption.Shape Dot.Circle
                           , Dot.NodeOption.Label [(Int.toString var, Dot.Center)] ]
      fun return (x, (i, s)) = (#name x, (i + 1, x :: s))
      val go : string Bdd.view * (int * Dot.node list) -> string * (int * Dot.node list) =
       fn (Bdd.False, s) => return ({name = "L", options = leafOptions, successors = []}, s)
        | (Bdd.True, s) => return ({name = "H", options = leafOptions, successors = []}, s)
        | (Bdd.If {var, edge0, edge1}, s) =>
          return ({ name = Int.toString (#1 s)
                  , options = varOptions var
                  , successors = [ {name = edge0, options = [Dot.EdgeOption.Style Dot.Solid]}
                                 , {name = edge1, options = [Dot.EdgeOption.Style Dot.Dashed]} ]}
                 , s)
    in
      Dot.layout { nodes = #2 (#2 (Bdd.fold (man, go, (2, []), t)))
                 , options = graphOptions
                 , title = "BDD"}
    end

end

end

(* Convenient, safe calculator-style API.
   Global manager. *)
structure Bdd =
struct

local
  structure Bdd = Bdd_impl

  type man = Bdd.man
  type t = Bdd.t

  val man: man = Bdd.new 100

in

type var = Bdd.var

val compare: t * t -> General.order = Bdd.compare

val ff : t = Bdd.ff
val tt : t = Bdd.tt

fun is_ff (t: t) : bool = Bdd.compare (t, Bdd.ff) = EQUAL
fun is_tt (t: t) : bool = Bdd.compare (t, Bdd.tt) = EQUAL

fun var (v: var) : t = Bdd.var (man, v)

val not : t -> t = Bdd.not_
fun op && (t0: t, t1: t) : t = Bdd.and_ (man, t0, t1)
fun op || (t0: t, t1: t) : t = Bdd.or (man, t0, t1)
fun nand (t0: t, t1: t) : t = Bdd.nand (man, t0, t1)
fun implies (t0: t, t1: t) : t = Bdd.implies (man, t0, t1)
fun xor (t0: t, t1: t) : t = Bdd.xor (man, t0, t1)
fun eq (t0: t, t1: t) : t = Bdd.eq (man, t0, t1)
fun nxor (t0: t, t1: t) : t = Bdd.nxor (man, t0, t1)

type support = Bdd.support

fun support (t: t) : support = Bdd.support (man, t)

fun exists (support: support, t: t) : t =
    Bdd.exists (man, support, t)

datatype view = datatype Bdd.view

fun fold (f: 'a view * 'b -> 'a * 'b, s: 'b, t: t) : 'a * 'b =
    Bdd.fold (man, f, s, t)

val view: t -> t view = Bdd.view

fun toDot (t: t): Layout.t = Bdd_ext.toDot (man, t)

end

end

(*
(* FIXME test code *)
open Bdd;

val a = var 0;
val b = var 1;
val c = var 2;

val bdd = xor (a, b);

val l = toDot bdd;
Layout.print (l, print);
print "\n";
*)
