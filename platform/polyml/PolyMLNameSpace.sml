(* Boilerplate, cobbled together from various people's efforts on the Poly/ML mailing list. *)
(* Get "use" to output directory information for the benefit of emacs. *)
(* Somewhat inspired by David Matthews's regression script "RunTests.sml" for Poly/ML *)

signature POLYMLNAMESPACE =
sig
  type t = PolyML.NameSpace.nameSpace

  val use_with_params : PolyML.Compiler.compilerParameters list -> string -> unit
  val eval_with_params : PolyML.Compiler.compilerParameters list -> string -> unit

  val new_namespace : t -> t

  (* "use" and "eval" are sensitive to these functions. *)
  val set_namespace : t -> unit
  val use_global_namespace : unit -> unit
  val use : string -> unit
  val eval : string -> unit
end

structure PolyMLNameSpace :> POLYMLNAMESPACE =
struct

type t = PolyML.NameSpace.nameSpace

structure Fs = OS.FileSys

(* FIXME HACK *)
structure LibrarySupport =
struct

(* Re-raise an exception that has been handled preserving the location. *)
fun reraise exn =
    case PolyML.exceptionLocation exn of
        NONE => raise exn
      | SOME location => PolyML.raiseWithLocation (exn, location);

end

(* Same as Poly/ML's built-in "use", but parameterised by compiler parameters *)
fun use_with_params (params: PolyML.Compiler.compilerParameters list) (originalName: string): unit =
    let
      (* use "f" first tries to open "f" but if that fails it tries "f.ML", "f.sml" etc. *)
      val suffixes = ref ["", ".ML", ".sml"];

      (* We use the functional layer and a reference here rather than TextIO.input1 because
         that requires locking round every read to make it thread-safe.  We know there's
         only one thread accessing the stream so we don't need it here. *)
      fun trySuffixes [] =
          (* Not found - attempt to open the original and pass back the
             exception. *)
          (TextIO.getInstream(TextIO.openIn originalName), originalName)
       |  trySuffixes (s::l) =
          (TextIO.getInstream(TextIO.openIn (originalName ^ s)), originalName ^ s)
          handle IO.Io _ => trySuffixes l
      (* First in list is the name with no suffix. *)
      val (inStream, fileName) = trySuffixes("" :: ! suffixes)
      val stream = ref inStream

      val lineNo   = ref 1;
      fun getChar () : char option =
          case TextIO.StreamIO.input1 (! stream) of
              NONE => NONE
           |   SOME (eoln as #"\n", strm) =>
               (
                 lineNo := !lineNo + 1;
                 stream := strm;
                 SOME eoln
               )
           |   SOME(c, strm) => (stream := strm; SOME c)
      open PolyML.Compiler
    in
      while not (TextIO.StreamIO.endOfStream(!stream)) do
            let
              val code = PolyML.compiler(getChar, params @ [CPFileName fileName, CPLineNo(fn () => !lineNo)])
                         handle exn =>
                                ( TextIO.StreamIO.closeIn(!stream); LibrarySupport.reraise exn )
            in
              code() handle exn =>
                            (
                              (* Report exceptions in running code. *)
                              TextIO.print ("Exception- " ^ exnMessage exn ^ " raised\n");
                              TextIO.StreamIO.closeIn (! stream);
                              LibrarySupport.reraise exn
                            )
            end;
      (* Normal termination: close the stream. *)
      TextIO.StreamIO.closeIn (! stream)
    end (* use_with_params *)

fun eval_with_params params text =
  let
    fun print s = (TextIO.output (TextIO.stdOut, s); TextIO.flushOut TextIO.stdOut);

    val line = ref 1;
    val in_buffer = ref (String.explode text);
    val out_buffer = ref ([]: string list);

    fun output () = String.concat (rev (! out_buffer));

    fun get () =
      (case ! in_buffer of
        [] => NONE
      | c :: cs => (in_buffer := cs; if c = #"\n" then line := ! line + 1 else (); SOME c));
    fun put s = out_buffer := s :: ! out_buffer;
    fun put_message {message = msg1, hard, location = {startLine = line, ...}, context} =
     (put (if hard then "Error: " else "Warning: ");
      PolyML.prettyPrint (put, 76) msg1;
      (case context of NONE => () | SOME msg2 => PolyML.prettyPrint (put, 76) msg2);
      put ("Line " ^ Int.toString line ^ "\n"));

    val parameters =
     [PolyML.Compiler.CPOutStream put,
      PolyML.Compiler.CPErrorMessageProc put_message,
      PolyML.Compiler.CPLineNo (fn () => ! line)];
    val _ =
      (while not (List.null (! in_buffer)) do
        PolyML.compiler (get, params @ parameters) ())
      handle exn =>
        (put ("Exception- " ^ General.exnMessage exn ^ " raised");
          print (output ()); raise exn);
  in print (output ()) end;

open PolyML.Compiler

(* Create a private name space from the given one. *)
local
  val inv = ref 0 : int ref
in

fun new_namespace
      ({ lookupFix, lookupSig, lookupVal, lookupType, lookupFunct, lookupStruct,
         allFix, allSig, allVal, allType, allFunct, allStruct, ...} : PolyML.NameSpace.nameSpace)
    : PolyML.NameSpace.nameSpace =
    let val i = !inv
        val () = inv := !inv + 1
        (* val () = print (">> NEW NAMEDSPACE: " ^ Int.toString i ^ " <<\n") *)
        fun makeSpace (str, globalLook, globalAll) =
            let open HashArray
                val table = hash 10
                fun lookup s =
                    ( (* print ("makeSpace.lookup(" ^ str ^ Int.toString i ^ "): " ^ s ^ "\n"); *)
                      case sub (table, s) of
                          NONE => ( (* print "NOT HERE GO GLOBAL\n"; *) globalLook s )
                        | SOME r => ( (* print "FOUND STUFF\n"; *) SOME r ) )
                fun enter(s, v) = ( (* print ("makeSpace.enter (" ^ str ^ Int.toString i ^ "): " ^ s ^ "\n"); *) update(table, s, v) )
                fun all () = fold (fn(s, v, l) => (s, v) :: l) (globalAll()) table
            in
              { lookup = lookup, enter = enter, all = all }
            end

        val fixSpace = makeSpace("fix", lookupFix, allFix)
        val sigSpace = makeSpace("sig", lookupSig, allSig)
        val valSpace = makeSpace("val", lookupVal, allVal)
        val typeSpace = makeSpace("type", lookupType, allType)
        val funSpace = makeSpace("fun", lookupFunct, allFunct)
        val strSpace = makeSpace("str", lookupStruct, allStruct)
    in
      {
        lookupFix    = #lookup fixSpace,
        lookupSig    = #lookup sigSpace,
        lookupVal    = #lookup valSpace,
        lookupType   = #lookup typeSpace,
        lookupFunct  = #lookup funSpace,
        lookupStruct = #lookup strSpace,
        enterFix     = #enter fixSpace,
        enterSig     = #enter sigSpace,
        enterVal     = #enter valSpace,
        enterType    = #enter typeSpace,
        enterFunct   = #enter funSpace,
        enterStruct  = #enter strSpace,
        allFix       = #all fixSpace,
        allSig       = #all sigSpace,
        allVal       = #all valSpace,
        allType      = #all typeSpace,
        allFunct     = #all funSpace,
        allStruct    = #all strSpace
      }
    end

end

(* FIXME probably should be more careful here: modules are generative,
   so loading this file several times might confuse things. *)
val namespace = ref NONE : PolyML.NameSpace.nameSpace option ref

fun set_namespace (ns : PolyML.NameSpace.nameSpace) : unit =
    namespace := SOME ns

fun use_global_namespace () : unit =
    namespace := NONE

fun use file =
    let val params =
            case !namespace of
                NONE => ((* print "Use global namespace.\n"; *) [])
              | SOME ns => ((* print "Use local namespace.\n"; *) [CPNameSpace ns])
    in
      case OS.Path.splitDirFile file of
          {dir = "", file} => use_with_params params file
        | {dir, file}      =>
          let val curDir = Fs.getDir ()
          in (
            app print ["\n", "Entering directory `", curDir, "/", dir, "'\n"];
            Fs.chDir dir;
            use_with_params params file;
            app print ["\n", "Leaving directory `", curDir, "/", dir, "'\n"];
            Fs.chDir curDir
          ) handle
             e => (Fs.chDir curDir; raise e)
          end
    end

fun eval str =
    let val params =
            case !namespace of
                NONE => (print "Use global namespace.\n"; [])
              | SOME ns => (print "Use local namespace.\n"; [CPNameSpace ns])
    in
      eval_with_params params str
    end

end;

(* Clobber the global "use" function. *)
val use = PolyMLNameSpace.use;
