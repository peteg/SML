(* Generic logging.
   Inspired by https://github.com/dbuenzli/logs

   Logging seems to be essentially globally imperative/an aspect:
    - we want to be able to log anywhere and everywhere, so no chance of passing a handle.
    - probably want logging ops to be synchronous and atomic to improve debugability.

   FIXME look at ocaml: http://erratique.ch/software/logs/doc/Logs.html

   FIXME handle unprintable characters in logged strings.

   FIXME add mechanism to filter based on name?

   FIXME define variants without thunks. Often we use constant strings.
 *)

signature LOG =
sig

  datatype t
    = App
    | Error
    | Warning
    | Info
    | Debug

  type src = { name : string, level : t }

  val enum : t -> int
  val compare : t Cmp.t
  val to_string : t -> string

  val src_to_string : src -> string

  type logger = { src : src, str : string } -> unit

  val no_logger : logger

  val set_level : t -> unit
  val set_logger : logger -> unit

  val guard : t -> ('a -> unit) -> 'a -> unit
  val log : src -> (unit -> string) -> unit

  val fns : string -> { app : (unit -> string) -> unit
                     , error : (unit -> string) -> unit
                     , warning : (unit -> string) -> unit
                     , info : (unit -> string) -> unit
                     , debug : (unit -> string) -> unit
                     }

end

structure Log :> LOG =
struct

datatype t
  = App
  | Error
  | Warning
  | Info
  | Debug

val enum : t -> int =
 fn App => 0
  | Error => 1
  | Warning => 2
  | Info => 3
  | Debug => 4

val compare : t Cmp.t =
 fn (l0, l1) => Int.compare (enum l0, enum l1)

val to_string : t -> string =
 fn App => "App"
  | Error => "Error"
  | Warning => "Warning"
  | Info => "Info"
  | Debug => "Debug"

type src = { name : string, level : t }

fun src_to_string ({name, level} : src) : string =
  let
    val d = Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeLocal (Time.now()))
  in
    "[" ^ d ^ " " ^ to_string level ^ "] " ^ name ^ ": "
  end

type logger = { src : src, str : string } -> unit

val no_logger : logger =
 fn _ => ()

val stdout_logger : logger =
 fn {src, str} =>
    let in
      print (src_to_string src ^ str ^ "\n");
      TextIO.flushOut TextIO.stdOut
    end

val level = ref Info
val logger = ref stdout_logger

fun set_level l = level := l
fun set_logger l = logger := l

fun guard l' f x : unit =
    case compare (l', !level) of
        GREATER => ()
      | _ => f x

fun log src f : unit =
  guard (#level src)
        (fn () => !logger {src=src, str=f ()})
        ()

fun fns name =
  let
    fun l level = log {name=name, level=level}
  in
    { app = l App
    , error = l Error
    , warning = l Warning
    , info = l Info
    , debug = l Debug
    }
  end

end
