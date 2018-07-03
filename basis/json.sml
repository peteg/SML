(* JSON marshalling.

   Some of this is from the MLton basis library;
   see http://mlton.org/License

FIXME is StringCvt efficient?
FIXME expect few fields in an Object

FIXME validate against
  http://seriot.ch/parsing_json.html
  -> https://github.com/nst/JSONTestSuite

*)

signature JSON =
sig

  datatype t
    = String of string
    | Int of IntInf.int
    | Real of Real.real
    | Object of fields
    | Array of t list
    | Bool of bool
    | Null
  withtype fields = t StringAList.t (* Symtab.table *)

  type 'a constructors
       = { String : string -> 'a
         , Int : IntInf.int -> 'a
         , Real : Real.real -> 'a
         , Object : 'a StringAList.t -> 'a (* 'a Symtab.table -> 'a *)
         , Array : 'a list -> 'a
         , Bool : bool -> 'a
         , Null : 'a
         }

  val empty : t (* {} *)
  val object : (string * t) list -> t

  val constructors : t constructors
  val scan : 'a constructors -> (char, 'b) StringCvt.reader -> ('a, 'b) StringCvt.reader

  val to_buffer : t -> Buffer.t
  val to_string : t -> String.t
  val from_string : 'a constructors -> string -> 'a option

  val render : t -> Buffer.t CoSeq.t

end

structure Json : JSON = (* FIXME why don't constructors leak through this translucent ascription? *)
struct

datatype t
  = String of string
  | Int of IntInf.int
  | Real of Real.real
  | Object of fields
  | Array of t list
  | Bool of bool
  | Null
withtype fields = t StringAList.t (* Symtab.table *)

type 'a constructors
     = { String : string -> 'a
       , Int : IntInf.int -> 'a
       , Real : Real.real -> 'a
       , Object : 'a StringAList.t -> 'a (* 'a Symtab.table -> 'a *)
       , Array : 'a list -> 'a
       , Bool : bool -> 'a
       , Null : 'a
       }

val constructors : t constructors
    = { String = String
      , Int = Int
      , Real = Real
      , Object = Object
      , Array = Array
      , Bool = Bool
      , Null = Null
      }

val empty : t = Object StringAList.empty
val object = Object o StringAList.from_list

(* FIXME improve use of the buffer, or use PP + tweak to use a buffer *)
fun to_string (String s) : String.t =
  let
    fun esc c = "\\u" ^ StringCvt.padLeft #"0" 4 (Int.toString (Char.ord c))
    fun trans #"\"" = "\\\""
      | trans #"\\" = "\\\\"
      | trans c =
        if Char.isPrint c
        then String.str c
        else esc c
  in
    "\"" ^ String.translate trans s ^ "\""
  end
  | to_string (Int i) = IntInf.toString i
  | to_string (Real r) = Real.toString r
  | to_string (Object os) =
    let
      fun trans (k, v) = to_string (String k) ^ ":" ^ to_string v
      val items = List.map trans (StringAList.to_list os) (* (Symtab.dest os) (* somewhat horrible *) *)
    in
      "{" ^ String.concatWith "," items ^ "}"
    end
  | to_string (Array js) = "[" ^ String.concatWith "," (map to_string js) ^ "]"
  | to_string (Bool true) = "true"
  | to_string (Bool false) = "false"
  | to_string (Null) = "null"

(* FIXME horrible *)
val to_buffer = Buffer.string o to_string
val render = CoSeq.return o to_buffer

(* SML/NJ ParserComb(inator)-based JSON parser. *)

structure PC = ParserComb

fun hex4 reader state =
  let
    fun loop (0, acc, state) = SOME (acc, state)
      | loop (i, acc, state) =
        case reader state of
            NONE => NONE
          | SOME (c, state) =>
            case Int.fromString (String.str c) of
                NONE => NONE
              | SOME n => loop (i - 1, n + acc * 16, state)
  in
    loop (4, 0, state)
  end

fun scanChar reader state =
  let
    fun escape state =
      case reader state of
          NONE => NONE
        | SOME (c, state') =>
          let
            fun yes c = SOME (c, state')
          in
            case c of
                  #"\"" => yes #"\""   | #"\\" => yes #"\\"
                | #"/" => yes #"/"     | #"b" => yes #"\b"
                | #"f" => yes #"\f"    | #"n" => yes #"\n"
                | #"r" => yes #"\r"    | #"t" => yes #"\t"
                | #"u" => (
                  case hex4 reader state' of
                      NONE => NONE
                    | SOME (d, s') => (SOME (Char.chr d, s')
                                      handle Chr => NONE))
                | _ => NONE
          end
  in
    case reader state of
        NONE => NONE
      | SOME (c, state) =>
        case c of #"\\" => escape state
                | #"\"" => NONE
                | _ => SOME (c, state)
  end


fun scanString reader state =
  let
    fun loop (state, cs) =
      case scanChar reader state of
          NONE => SOME (implode (rev cs), state)
        | SOME (c, state) => loop (state, c :: cs)
  in
    loop (state, [])
  end

fun seqWith3 f (a, b, c) =
  PC.seqWith
    (fn (x, (y, z)) => f (x, y, z))
    (a, PC.seq (b, c))

fun whitespace r = PC.zeroOrMore (PC.eatChar Char.isSpace) r

fun scanQString r =
  seqWith3 #2 (PC.char #"\"", scanString, PC.char #"\"") r

fun scanValue (cs : 'a constructors) : ('a, 'b) PC.parser = fn r =>
  seqWith3 #2 (
    whitespace,
    PC.or' [
      PC.wrap (scanQString, #String cs),
      PC.wrap (scanArr cs, #Array cs),
      PC.wrap (scanObj cs, #Object cs),
      PC.wrap (PC.string "null", fn _ => #Null cs),
      PC.wrap (PC.string "true", fn _ => #Bool cs true),
      PC.wrap (PC.string "false", fn _ => #Bool cs false),
      PC.wrap (IntInf.scan StringCvt.DEC, #Int cs),
      PC.wrap (Real.scan, #Real cs) (* FIXME suspicious: do the SML/JS formats for floats line up? *)
    ],
    whitespace
  ) r

and scanArr (cs : 'a constructors) : ('a list, 'b) PC.parser = fn r =>
    PC.seqWith #2 (
      PC.char #"[",
      PC.or (
        PC.wrap (PC.seq (whitespace, PC.char #"]"), fn _ => nil),
        PC.seqWith (op ::) (
          scanValue cs,
          PC.seqWith #1 (
            PC.zeroOrMore (PC.seqWith #2 (PC.char #",", scanValue cs)),
            PC.eatChar (fn c => (c = #"]"))
          )
        )
      )
    ) r

and scanKeyValue cs : (string * 'a, 'b) PC.parser = fn r =>
    PC.seq (
      seqWith3 #2 (whitespace, scanQString, whitespace),
      PC.seqWith #2 (PC.char #":", scanValue cs)
    ) r

and scanObj cs r =
    let
      fun map_together (first, rest) =
        StringAList.insert (K (#2 first)) (#1 first) (StringAList.from_list rest) (* Symtab.update_new first (Symtab.make rest) *)
    in
      PC.seqWith #2 (
        PC.char #"{",
        PC.or (
          PC.wrap (PC.seq (whitespace, PC.char #"}"), fn _ => StringAList.empty (* Symtab.empty *)),
          PC.seqWith map_together (
            scanKeyValue cs,
            PC.seqWith #1 (
              PC.zeroOrMore (PC.seqWith #2 (PC.char #",", scanKeyValue cs)),
              PC.eatChar (fn c => (c = #"}"))
            )
          )
        )
      ) r
    end

val scan = scanValue

fun from_string cs = StringCvt.scanString (scan cs)

end
