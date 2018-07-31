(* Comfortable basic things.
   Some stuff from Isabelle, some from mltonlib.
 *)

type filename = string

val id = Fn.id
val const = Fn.const

val I = id
val K = const

val curry = Fn.curry
val uncurry = Fn.uncurry

fun recur (a, f) =
  let
    fun loop a = f (a, loop)
  in
    loop a
  end

(* A placeholder for to-be-implemented stuff. *)
exception FIXME of string

structure Order =
struct

type t = order

fun is_EQUAL x = x = EQUAL
fun is_GREATER x = x = GREATER
fun is_LESS x = x = LESS

val swap =
 fn LESS    => GREATER
  | EQUAL   => EQUAL
  | GREATER => LESS

val orWhenEq =
 fn (EQUAL, th) => th ()
  | (other,  _) => other

end

structure Cmp =
struct

type 'a t = 'a * 'a -> Order.t

fun map (f : 'a -> 'b) cmp (a, b) : 'b t =
  cmp (f a, f b)

fun rev (cmp : 'a t) xy : order =
  case cmp xy of LESS => GREATER | EQUAL => EQUAL | GREATER => LESS

(* Lexicographic combination *)
fun op *` (aO, bO) ((lA, lB), (rA, rB)) =
    case aO (lA, rA)
     of EQUAL => bO (lB, rB)
      | other => other

local
  open Order
in
fun mkRelOps cmp =
  {<  = is_LESS    o cmp, <= = not o is_GREATER o cmp,
   == = is_EQUAL   o cmp, != = not o is_EQUAL   o cmp,
   >  = is_GREATER o cmp, >= = not o is_LESS    o cmp}

local
  fun mk is cmp (x, y) = if is (cmp (x, y)) then y else x
in

fun max ? = mk is_LESS ?
fun min ? = mk is_GREATER ?

end

end

end

structure Bool =
struct

type t = bool

open Bool

fun compare (false, true) = LESS
  | compare (true, false) = GREATER
  | compare _ = EQUAL

val to_string = toString
val from_string = fromString

end

structure List =
struct

type 'a t = 'a list

open List

val compare = collate

val last : 'a list -> 'a =
 fn [] => raise List.Empty
  | [x] => x
  | _ :: xs => last xs

fun rev_map (f : 'a -> 'b) : 'a list -> 'b list =
  let
    fun go acc =
      fn [] => acc
      | x :: xs => go (f x :: acc) xs
  in
    go []
  end

fun mem (xs : ''a list) (x : ''a) : bool =
  List.exists (fn x' => x = x') xs

fun merge_drop_dups (cmp : 'a * 'a -> order) : 'a list * 'a list -> 'a list =
  fn ([], ys) => ys
  | (xs, []) => xs
  | (xxs as x :: xs, yys as y :: ys) =>
    case cmp (x, y) of
        LESS => x :: merge_drop_dups cmp (xs, yys)
      | EQUAL => merge_drop_dups cmp (xxs, ys)
      | GREATER => y :: merge_drop_dups cmp (xxs, ys)

fun to_string (a_to_string : 'a -> string) : 'a list -> string =
    String.concatWith " " o map a_to_string

fun push r x = r := x :: !r
fun pop r = case !r of x::xs => (r := xs ; SOME x) | [] => NONE

end

structure Int =
struct

type t = int

open Int

val to_string = toString
val from_string = fromString

end

structure Sum =
struct

datatype ('a, 'b) t = INL of 'a | INR of 'b

fun sum (f, g) = fn INL a => f a | INR b => g b
fun bimap (f, g) = sum (INL o f, INR o g)

(* Value-carrying-exceptions ala Haskell *)
fun fmap (g : 'b -> 'c) : ('a, 'b) t -> ('a, 'c) t = bimap (id, g)
fun bind e f =
    case e of
        INL a => INL a
      | INR b => f b

val return = INR
val fail = INL

fun compare (cmpA, cmpB) =
    fn (INL l, INL r) => cmpA (l, r)
  | (INL _, INR _) => LESS
  | (INR _, INL _) => GREATER
  | (INR l, INR r) => cmpB (l, r)

end

structure Option =
struct

type 'a t = 'a option

open Option

fun get_opt (none : 'a) : 'a option -> 'a =
  fn NONE => none
  | SOME x => x

val is_NONE =
 fn NONE   => true
  | SOME _ => false

val is_SOME =
 fn NONE   => false
  | SOME _ => true

fun compare cmp =
  fn (NONE,   NONE) => EQUAL
  | (SOME _, NONE)   => GREATER
  | (NONE,   SOME _) => LESS
  | (SOME a, SOME b) => cmp (a, b)

fun option (none, some) =
    fn NONE => none ()
  | SOME x => some x

fun the (SOME x) = x
  | the NONE = raise Option

end

structure OptM =
struct

type 'a t = 'a option

fun f >>= g = case f of NONE => NONE | SOME x => g x
val return = SOME
val fmap = Option.map

fun catch (b : unit -> 'b t) (h : unit -> 'b) : 'b =
  case b () of
      NONE => h ()
    | SOME b => b

end

structure Pair =
struct

fun swap (a, b) = (b, a)
fun swizzle ((a, b), (c, d)) = ((a, c), (b, d))

fun fst (a, _) = a
fun snd (_, b) = b

fun biapp (ea, eb) (a, b) = (ea a : unit ; eb b : unit)
fun appFst eA = biapp (eA, id)
fun appSnd eB = biapp (id, eB)

fun bimap (fa, fb) (a, b) = (fa a, fb b)
fun mapFst fA = bimap (fA, id)
fun mapSnd (fB : 'a -> 'b) : 'c * 'a -> 'c * 'b = bimap (id, fB)

local
  fun isFalse b = b = false
  fun isTrue b = b = true
  fun mk p (fA, fB) (a, b) = let
    val a = fA a
  in
    if p a then fB b else a
  end
in
fun all     x = mk isTrue   x
fun exists  x = mk isFalse  x
fun equal   x = mk isTrue   x o swizzle
fun compare x = mk Order.is_EQUAL x o swizzle
end

fun foldl (fa, fb) ((a, b), s) = fb (b, fa (a, s))
fun foldr (fa, fb) ((a, b), s) = fa (a, fb (b, s))

end

structure Ref (* : REF *) =
struct

type 'a t = 'a ref

val new = ref
val ! = !
val op := = op :=

fun op :=: (r1, r2) =
  let
    val v1 = !r1
    val v2 = !r2
  in
    r1 := v2; r2 := v1
  end

fun exchange (r, v) = !r before r := v

fun app ef = ef o !

fun map f = new o f o !

fun modify f r = r := f (!r)

val equal = op =

end

structure String =
struct

open String

type t = string

local

fun fast_string_ord (s1, s2) =
  if Platform.pointer_eq (s1, s2)
  then EQUAL
  else case Int.compare (size s1, size s2) of
           EQUAL => String.compare (s1, s2)
         | ord => ord
in
val compare : t Cmp.t = fast_string_ord
end

val to_hex : string -> string =
    String.translate (StringCvt.padLeft #"0" 2 o Int.fmt StringCvt.HEX o Char.ord)

end

structure Time =
struct

open Time

type t = time

end

structure Unit =
struct

type t = unit

val compare : t Cmp.t = K EQUAL

end

structure Date =
struct

open Date

fun int_to_month (i : int) : month =
    case i of
        1 => Jan
      | 2 => Feb
      | 3 => Mar
      | 4 => Apr
      | 5 => May
      | 6 => Jun
      | 7 => Jul
      | 8 => Aug
      | 9 => Sep
      | 10 => Oct
      | 11 => Nov
      | 12 => Dec
      | _ => raise Date

end

(*
   FIXME something like the inductive version of CoSeq (?).
   Perhaps generalize over carrier + share signature.
   What exactly is this for? See equivalent(s) in ocaml and Haskell.
*)
signature BUFFER =
sig
  type t
  val empty : t
  val char : char -> t
  val substring : substring -> t
  val string : string -> t
  val append : t -> t -> t
  val concat : t list -> t

  val size : t -> int
  val to_string : t -> string
  val output : t -> BinIO.outstream -> unit
end

structure Buffer :> BUFFER =
struct

(* FIXME make this imperative? This approach delays copying stuff. *)
(* FIXME cheesy maintain size as we go *)

datatype elt
  = Char of char
  | Substring of substring
  | String of string

type t = int * (elt list -> elt list) (* FIXME record probably more idiomatic *)

val empty = (0, I)
fun char c = (1, fn b => Char c :: b)

fun substring ss =
    if Substring.isEmpty ss
    then empty
    else (Substring.size ss, fn b => Substring ss :: b)

fun string "" = empty
  | string s = (String.size s, fn b => String s :: b)

fun append (szx, x) (szy, y) = (szx + szy, y o x)
val concat : t list -> t = List.foldl (fn (x, b) => append b x) empty

fun elt_to_string e =
    case e of
        Char c => String.str c
      | Substring ss => Substring.string ss
      | String s => s

fun to_list (b : t) : elt list = #2 b []

val size : t -> int = #1
val to_string = String.concat o List.rev_map elt_to_string o to_list (* FIXME see Poly libs for more efficient ways to do this *)
fun output b stream = List.app (fn s => BinIO.output (stream, Byte.stringToBytes (elt_to_string s))) (rev (to_list b))

end

(* FIXME top-level namespace pollution *)

(* http://stackoverflow.com/questions/11276985/emulating-try-with-finally-in-ocaml *)
(* FIXME look into the mltonlib extended basis. *)
(* Observe: CBV means x is not `protect`ed. Which is what we probably want for file ops. *)
fun unwind (protect: 'a -> 'b) (x: 'a) (f: 'a -> 'c) : 'c =
    let
      val res = Sum.INL (f x) handle e => Sum.INR e
      val ()  = ignore (protect x)
    in
      case res of
          Sum.INL y => y
        | Sum.INR e => Platform.reraise e
    end

(* FIXME presumably inefficient. Could make it safer by providing a limit. *)
fun read_line (in_stream : BinIO.instream) : string option =
    let
      fun finalize (cs : char list) : string option =
          if List.null cs
          then NONE
          else SOME (String.implode (List.rev cs))
      fun aux (cs : char list) : string option =
          case BinIO.input1 in_stream of
              NONE => finalize cs
            | SOME 0w10 => finalize cs
            | SOME b => aux (Byte.byteToChar b :: cs)
    in
      aux []
    end

fun contents (filename: filename) : string =
    unwind BinIO.closeIn
           (BinIO.openIn filename)
           (Byte.bytesToString o BinIO.inputAll)

fun writeFile (filename: filename) (contents: string) : unit =
    unwind BinIO.closeOut
           (BinIO.openOut filename)
           (fn stream => BinIO.output (stream, Byte.stringToBytes contents))

(* FIXME move. Strip whitespace at start and end too. Not TRC. *)
fun normalize str =
  let
    val n = size str
    fun eatSpace i =
      if i < n andalso Char.isSpace (String.sub (str, i))
      then eatSpace (i + 1)
      else i
    fun iter i =
      if i < n
      then
        let
          val c = String.sub (str, i)
        in
          if Char.isSpace c
          then #" " :: iter (eatSpace i)
          else c :: iter (i + 1)
        end
      else []
  in
    String.implode (iter (eatSpace 0))
  end

(* Normalize some smlnj-lib libraries a little *)
structure Fifo =
struct

open Fifo

type 'a t = 'a fifo

val is_empty = isEmpty

end

structure Queue =
struct

open Queue

type 'a t = 'a queue

val empty = mkQueue

end
