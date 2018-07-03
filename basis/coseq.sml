(* Lazy sequences using references.

   The Isabelle seq.ML module is non-strict but not lazy: it does not
   memoize. As we have side-effects we surely want memoization.

   Larry Paulson's in /ML for the Working Programmer/ is spine-lazy
   but forces elements too eagerly (the first element is forced when
   the sequence is constructed). This structure does not force
   elements until an eliminator is called.

These isses are discussed in: Wadler, MacQueen, Taha "How to add
laziness to a strict language without even being odd."

Note the memoization is in the `Susp` structure.

FIXME fixed points:
Wells, related work:
http://www.macs.hw.ac.uk/~jbw/papers/Turbak+Wells:Cycle-Therapy:A-Prescription-for-Fold-and-Unfold-on-Regular-Trees:PPDP-2001.pdf
Well, not really, but some ideas for fixed points. See also mltonlib's Tie.

*)

signature COSEQ =
sig
  type 'a t
  type 'a view = ('a * 'a t) option

  (* Constructors *)
  val empty : unit -> 'a t
  val cons : 'a * 'a t -> 'a t
  val unfold : ('s -> ('a * 's) option) * 's -> 'a t
  val from_list : 'a list -> 'a t
  val tabulate : (int -> 'a) -> 'a t

  (* Eliminators *)
  val view : 'a t -> 'a view
  val null : 'a t -> bool
  val app : ('a -> unit) -> 'a t -> unit
  val to_list : 'a t -> 'a list

  (* Classic functionals *)
  val append : 'a t * 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val concatl : 'a t list -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val fold : ('a * 'b -> 'b) * 'b * 'a t -> 'b (* diverges on infinite sequences *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_partial : ('a -> 'b option) -> 'a t -> 'b t
  val map2 : ('a * 'b -> 'c) * 'a t * 'b t -> 'c t (* raises ListPair.UnequalLengths *)
  val repeat : 'a -> 'a t
  val take : int -> 'a t -> 'a list

  (* Monadic wonderland *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val join : 'a t t -> 'a t
  val return : 'a -> 'a t

  (* Tying the knot *)
  exception Blackhole
  val fix : ('a t -> 'a t) -> 'a t

(* val interleave : 'a t * 'a t -> 'a t *)

  (* FIXME belongs where? *)
  val chunk: int -> Buffer.t t -> string t
end

structure CoSeq :> COSEQ =
struct

open Basics

datatype 'a t = CoIt of 'a view Susp.t
withtype 'a view = ('a * 'a t) option

(* Constructors *)

fun unfold (f: 's -> ('a * 's) option, s: 's) : 'a t =
  CoIt (Susp.delay (fn () => f s |> Option.map (Pair.mapSnd (fn s' => unfold (f, s')))))

fun empty () : 'a t = unfold (fn _ => NONE, ())

fun from_list xs =
  unfold (fn xxs => case xxs of [] => NONE | x :: xs => SOME (x, xs), xs)

fun tabulate f =
  unfold (fn i => SOME (f i, i + 1), 0)

(* Eliminators *)

fun view (CoIt cs: 'a t) : 'a view = Susp.force cs

fun null cs =
  case view cs of
      NONE => true
    | SOME _ => false

fun app (f : 'a -> unit) : 'a t -> unit =
  Option.app (fn (x, cs) => (f x; app f cs)) o view

fun concat (css: 'a t t) : 'a t =
  let
    fun aux sss =
      case Sum.sum (view, SOME) sss of
          NONE => NONE
        | SOME (s, ss) =>
          case view s of
              NONE => aux (Sum.INL ss)
            | SOME (x, s') => SOME (x, Sum.INR (s', ss))
  in
    unfold (aux, Sum.INL css)
  end

fun concatl (css: 'a t list) : 'a t =
  concat (from_list css)

fun to_list cs =
  let
    fun aux acc cs =
      case view cs of
          NONE => List.rev acc
        | SOME (x, cs') => aux (x :: acc) cs'
  in
    aux [] cs
  end

(* Classic functionals *)

fun append (cs, ds) = concat (from_list [cs, ds])

fun filter pred (xs: 'a t) : 'a t =
  let
    fun aux cs =
      case view cs of
          NONE => NONE
        | SOME (c, cs') =>
          if pred c then SOME (c, cs') else aux cs'
  in
    unfold (aux, xs)
  end

fun fold (f, z, cs) =
  case view cs of
      NONE => z
    | SOME (a, cs) => fold (f, f (a, z), cs)

fun map f xs = unfold (Option.map (Library.apfst f) o view, xs)
fun map2 (f, xs, ys) =
  let
    fun aux (SOME (x, xcs), SOME (y, ycs)) = SOME (f (x, y), (xcs, ycs))
      | aux (NONE, NONE) = NONE
      | aux _ = raise ListPair.UnequalLengths
  in
    unfold (aux o Pair.bimap (view, view), (xs, ys))
  end

fun map_partial f (xs: 'a t) : 'b t =
  let
    fun aux cs =
      case view cs of
          NONE => NONE
        | SOME (c, cs') =>
          (case f c of
               NONE => aux cs'
             | SOME c' => SOME (c', cs'))
  in
    unfold (aux, xs)
  end

fun repeat (x: 'a) : 'a t = unfold (fn _ => SOME (x, ()), ())

fun take i : 'a t -> 'a list =
  let
    fun aux i acc cs =
      if i <= 0
      then List.rev acc
      else
        case view cs of
            NONE => List.rev acc
          | SOME (x, cs') => aux (i - 1) (x :: acc) cs'
  in
    aux i []
  end

val join = concat
fun bind f g = join (map g f) (* FIXME optimise? *)
fun return x = from_list [x]

fun cons (x, xs) = append (return x, xs) (* FIXME optimise? *)

(* Backpatching. Would be nice to prove this works. A bit inefficient? *)
exception Blackhole

fun fix f =
  let
    val r = ref (CoIt (Susp.delay (fn () => raise Blackhole)))
    val str = CoIt (Susp.delay (fn () => view (!r)))
    val out = f str
  in
    r := out;
    out
  end

(* FIXME move to tests
val () = print ">>> coseq test <<<\n"
(* val xs = fix (fn s => case view s of NONE => empty () | SOME _ => empty ()) *)
(* FIXME might hope to blow up on blackhole, but loops *)
(* val xs = fix (map (fn i => i + 1)) *)
val xs = fix (cons 0 o map (fn i => i + 1))
val () = List.app (fn i => print (Int.to_string i ^ "\n")) (take 10 xs)
*)

fun chunk (chunk_size: int) (b: Buffer.t t) : string t =
  let
    fun aux b s =
      if Buffer.size b > chunk_size
      then (b, s)
      else
        case view s of
            NONE => (b, s)
          | SOME (x, s') => aux (Buffer.append b x) s'
    fun go s =
      let
        val (b, s') = aux Buffer.empty s
      in
        if Buffer.size b = 0
        then NONE
        else SOME (Buffer.to_string b, s')
      end
  in
    unfold (go, b)
  end

end
