(* A protocol for talking to Poly/ML.

Based on the existing IDE protocols in TopLevelPolyML.

FIXME extensible
FIXME encode with JSON to ease interaction with a web browser.
 - ideally want to parameterize over this

Want to support a persistent IDE-like thing and a REPL.
FIXME print -> debug log

FIXME discard response datatype: use a signature of fn calls instead.

Model for user code:
 - compile stuff from the IDE
  - while running, it can output stuff in an (interpreted) stdout kind of way
 - if it wants to be interactive, then it should:
  - fork it's own thread (or we do that for it)
  - and/or set up it's own websocket / http handler
  - and when done, output something on the pseudo-stdout to tell the browser to hit that up

*)

(* FIXME notionally as much of the SML I/O stack as makes sense.
   Would be nice to be agnostic about JSON, HTML, ...
 *)
signature POLYML_PROTOCOL_IO =
sig

  val print : string -> unit
  val json : Json.t -> unit

  (* FIXME block until available. Terminate thread if the websocket thread dies. *)
  val get : unit -> Json.t

end

signature POLYML_PROTOCOL =
sig

  val handle_polyml_ide : Http_websockets.handler

  structure IO : POLYML_PROTOCOL_IO

end

structure Polyml_protocol :> POLYML_PROTOCOL =
struct

open Basics (* FIXME eliminate *)
open Thread

val VERSION = "0.0.1"

type basicLoc = (* Locations in request packets. *) { startOffset: FixedInt.int, endOffset: FixedInt.int }
type compile_error = { hardError: bool, location: PolyML.location, message: PolyML.pretty }

datatype direction = DirUp | DirLeft | DirRight | DirDown

datatype dectype = DecLocal | DecOpen | DecParent

datatype request =
         (* Requests sent by the IDE to Poly/ML. *)
        HelloRequest
            of { requestId: string }
    |   PropertyRequest (* O *)
            of { requestId: string, parseTreeId: string, location: basicLoc }
    |   MoveRequest (* M *)
            of { requestId: string, parseTreeId: string, location: basicLoc, direction: direction }
    |   TypeRequest (* T *)
            of { requestId: string, parseTreeId: string, location: basicLoc }
    |   DecRequest (* I *)
            of { requestId: string, parseTreeId: string, location: basicLoc, decType: dectype }
    |   RefRequest (* V *)
            of { requestId: string, parseTreeId: string, location: basicLoc }
    |   CompileRequest (* R *)
            of { requestId: string, fileName: string, startPosition: FixedInt.int,
                 preludeCode: string, sourceCode: string }
    |   KillRequest (* K *)
            of { requestId: string }
    |   UserRequest
            of { requestId: string, payload: Json.t }
    |   UnknownRequest (* Provided for upwards compatibility. *)
            of { request: int, requestId: string}

(* FIXME factor this more: the dispatch should be pure data, and this is an I/O wrapper... or who cares? This is JSON specific. *)
fun read_request (recv : unit -> Http_websockets.packet_type * Word8Vector.vector) : request option =
  let
    val (packet_type, vec) = recv ()
    val () = if packet_type <> Http_websockets.TXT then raise FIXME "Got a non-TXT websocket packet." else ()
    val str = Byte.bytesToString vec
    val constructors = Json.constructors (* FIXME bespoke type? *)
    val request = the (Json.from_string constructors str)
    val () = print ("Polyml_protocol/read_request: '" ^ Json.to_string request ^ "'\n")
    val (Json.String tag, fs) =
        case request of
            Json.Object fs => (the (StringAList.lookup fs "tag"), fs) (* FIXME nuke the *)
          | _ => raise FIXME "JSON missing tag field"

 (* FIXME like the DB code *)
    fun FIXME_lookup_str f =
      case StringAList.lookup fs f of
          SOME (Json.String s) => s
        | _ => raise FIXME ("FIXME_lookup_str: missing field: '" ^ f ^ "'")
    fun FIXME_lookup_int f = case StringAList.lookup fs f of SOME (Json.Int i) => Int.fromLarge i
    fun FIXME_lookup_object f = case StringAList.lookup fs f of SOME json => json
  in
    case tag of
        "Hello" =>
        SOME (HelloRequest { requestId = FIXME_lookup_str "requestId" })
      | "Compile" =>
        SOME (CompileRequest { requestId = FIXME_lookup_str "requestId"
                             , fileName = FIXME_lookup_str "fileName"
                             , startPosition = FIXME_lookup_int "startPosition"
                             , preludeCode = FIXME_lookup_str "preludeCode"
                             , sourceCode = FIXME_lookup_str "sourceCode" })
      | "Kill" =>
        SOME (KillRequest { requestId = FIXME_lookup_str "requestId" })
      | "UserRequest" =>
        SOME (UserRequest { requestId = FIXME_lookup_str "requestId"
                          , payload = FIXME_lookup_object "payload" })
      | _ => raise FIXME ("Polyml_protocol/read_request: Unknown tag: '" ^ tag ^ "'")
      (* | _ => SOME (UnknownRequest { request = 3, requestId = FIXME_lookup_str "requestId" }) *)
  end

datatype compile_result =
        Succeeded of compile_error list
    |   RuntimeException of PolyML.pretty * compile_error list
    |   PreludeFail of string
    |   CompileFail of compile_error list
    |   CompileCancelled of compile_error list

datatype response =
        (* Replies sent from Poly/ML to the IDE. *)
        HelloResponse
            of { requestId: string, version: string }
    |   PropertyResponse (* O *)
            of { requestId: string, parseTreeId: string, location: basicLoc, commands: string list }
    |   MoveResponse  (* M *)
            of { requestId: string, parseTreeId: string, location: basicLoc }
    |   TypeResponse (* T *)
            of { requestId: string, parseTreeId: string, location: basicLoc, typeRes: PolyML.pretty option }
    |   DecResponse (* I *)
            of { requestId: string, parseTreeId: string, location: basicLoc,
                 decLocation: PolyML.location option }
    |   RefResponse (* V *)
            of { requestId: string, parseTreeId: string, location: basicLoc, references: basicLoc list }
    |   CompilerResponse (* R *)
            of { requestId: string, parseTreeId: string, finalOffset: FixedInt.int, result: compile_result }
    |   UnknownResponse (* Provided for upwards compatibility. *)
            of { request: int, requestId: string }

(* JSON specific *)
val pretty_to_json : PolyML.pretty -> Json.t =
 fn pretty =>
   let
     val buffer = ref Buffer.empty
     fun put str = buffer := Buffer.append (!buffer) (Buffer.string str)
   in
     PolyML.prettyPrint (put, 80) pretty; (* FIXME 80 magic *)
     Json.String (Buffer.to_string (!buffer))
   end

val location_to_json : PolyML.location -> Json.t =
    Json.object o
    (fn { file, startLine, startPosition, endLine, endPosition } =>
        [ ("file", Json.String file)
        , ("startLine", Json.Int (Int.toLarge startLine))
        , ("startPosition", Json.Int (Int.toLarge startPosition))
        , ("endLine", Json.Int (Int.toLarge endLine))
        , ("endPosition", Json.Int (Int.toLarge endPosition)) ])

val compile_error_to_json : compile_error -> Json.t =
    Json.object o
    (fn { hardError, location, message } =>
        [ ("hardError", Json.Bool hardError)
        , ("location", location_to_json location)
        , ("message", pretty_to_json message) ])

val compile_result_to_json =
    Json.object o
    (fn Succeeded compile_errors =>
        [ ("tag", Json.String "Succeeded")
        , ("compile_errors", Json.Array (List.map compile_error_to_json compile_errors)) ]
    | RuntimeException (pretty, compile_errors) =>
        [ ("tag", Json.String "RuntimeException")
        , ("string", pretty_to_json pretty)
        , ("compile_errors", Json.Array (List.map compile_error_to_json compile_errors)) ]
    | PreludeFail str =>
        [ ("tag", Json.String "PreludeFail")
        , ("string", Json.String str) ]
    | CompileFail compile_errors =>
        [ ("tag", Json.String "CompileFail")
        , ("compile_errors", Json.Array (List.map compile_error_to_json compile_errors)) ]
    | CompileCancelled compile_errors =>
        [ ("tag", Json.String "CompileCancelled")
        , ("compile_errors", Json.Array (List.map compile_error_to_json compile_errors)) ]
    )

(* FIXME push the CoSeq further up the chain? *)
fun send_response send (response : response) : unit =
  let
    fun send_json json_fields =
      let
        val str = Json.to_string (Json.Object json_fields)
        val () = print ("Sending response: '" ^ str ^ "'\n") (* FIXME log *)
      in
        send (Http_websockets.TXT, CoSeq.return str)
      end
  in
    case response of
        HelloResponse { requestId, version } =>
          send_json (StringAList.from_list [ ("tag", Json.String "Hello")
                                           , ("requestId", Json.String requestId)
                                           , ("version", Json.String version)])
      | CompilerResponse { requestId, parseTreeId, finalOffset, result } =>
          send_json (StringAList.from_list [ ("tag", Json.String "CompilerResponse")
                                           , ("requestId", Json.String requestId)
                                           , ("parseTreeId", Json.String parseTreeId)
                                           , ("finalOffset", Json.Int (Int.toLarge finalOffset))
                                           , ("result", compile_result_to_json result) ])
      | _ => raise FIXME "send_response: unimplemented."
  end


(* FIXME If anyone is impatient, dump their stuff on stdout. *)
val user_output_send : (Http_websockets.packet_type * string CoSeq.t -> unit) ref =
    ref (fn (packet_type, cs) => CoSeq.app print cs)

val user_input_queue =
    Fifo_channel.create ()

structure IO =
struct

fun json (json : Json.t) : unit =
  let
    val json = Json.object [ ("tag", Json.String "UserData")
                           , ("payload", json ) ]
  in
    !user_output_send ( Http_websockets.TXT
                      , CoSeq.chunk 16384 (Json.render json)) (* FIXME magic *)
  end

fun print str : unit =
  json (Json.object [ ("tag", Json.String "STDOUT")
                    , ("payload", Json.String str) ])

fun get () : Json.t =
  the (Fifo_channel.get (user_input_queue, NONE))

fun put (json : Json.t) : unit =
  Fifo_channel.put (user_input_queue, json)

end

(* **************************************** *)
(* JSON agnostic *)

(* FIXME try to encapsulate the entire compiler context here *)
(* FIXME try to encapsulate thread-safety of the compiler here. *)

(* FIXME or just use a continuation everywhere? *)
datatype outcome
  = Continue of response option
  | Thread of { requestId: string, f: unit -> response }
type state = (string * Thread.thread) option
type process = (outcome, state) StateM.m

(* FIXME I don't understand the management of parse trees.
   I think this might make sense as a separate structure, perhaps the entire compilation request code
   My intuition is that we'll load entire buffers in at a time.
   So the prelude idea is useful (if we have context) but what is this for?
 *)
val parseTree = ref ("", []) (* Parsetree ID and parsetrees as a list. *)

(* Save the last parsetree here. *)
val lastParsetree =
    ref (case parseTree of ref(_, hd::_) => SOME hd | _ => NONE)

(* Access the parse tree and other information with the lock held. *)
local
  val parseLock = Mutex.mutex ()
in
fun withLock f = ThreadLib.protect parseLock f ()
(*
         let
           open Thread.Thread Thread.Mutex
           val originalState = getAttributes()
           val () = setAttributes[InterruptState InterruptDefer]
           val () = lock parseLock
           val result = f ()
           val () = unlock parseLock
           val () = setAttributes originalState
         in
           result
         end
*)
  end

(* Get the current parse tree and identifier. *)
fun getCurrentParse () =
  withLock (fn () => let val (id, trees) = ! parseTree in (trees, ! lastParsetree, id) end)

(* Update lastParsetree if the id is still valid. *)
fun updateLastParse(id, pt) =
  withLock (fn () => if id = #1 (! parseTree) then lastParsetree := pt else ())

(* Set parse tree and ID as a result of a compilation.  Sets lastParsetree to the
   head of the updated parse tree. *)
fun setParseTree(pt, id) =
  let
    fun f () =
      (
        parseTree := (id, pt);
        case pt of
            [] => lastParsetree := NONE
         |   hd :: _ => lastParsetree := SOME hd
      )
  in
    withLock f
  end

(* The source text may consist of several "programs".  When we compile a "program" we
   have to provide a way for the parsetree for this "program" to navigate to others
   even though they won't have been compiled yet.  This enables it to work. *)
(* We have to return functions for the parent, for the next sibling even if there
   isn't one and for the previous sibling. *)
fun toplevelParseTree (parseRootRef as ref currentList) =
  let
    open PolyML

    (* This is called when we have processed the previous "programs" but
               not yet processed this one. *)
    fun makelist([], _) = (* Shouldn't happen *) raise Fail "Null list"
    |   makelist(l as (locn, props) :: tl, previous) =
        let
          fun this () = makelist(l, previous)
          (* If there is another item in the list we need a
                       property that moves there whose "previous" property
                       comes here. *)
          val next =
              case tl of
                  [] => []
               |   _ => [PTnextSibling(
                           fn () => makelist(tl, [PTpreviousSibling this]))]
        in
          (locn, previous @ next @ props)
        end

    fun parent () =
      case ! parseRootRef of
          [] => raise Fail "Empty Tree"
       |   trees as (hd :: _) =>
           let
             (* Navigation for one or more topdecs. *)
             val fullLoc =
                 case (hd, List.last trees) of
                     (({ file, startLine, startPosition, ... }, _),
                      ({ endLine, endPosition, ... }, _)) =>
                     {
                       file=file, startLine=startLine,
                       startPosition=startPosition,
                       endLine=endLine, endPosition=endPosition
                     }
           in
             (fullLoc, [PTfirstChild(fn () => makelist(trees, []))])
           end

    val itemCount = List.length currentList

    fun moveToNth n =
      let
        fun move (tree, 0) = tree
        |   move ((loc, opts), n) =
            case List.find(fn PTnextSibling _ => true | _ => false) opts of
                NONE =>
                let
                  (* We have to put a dummy item in at the end since when we
                               created the parent properties for the last "program" we will
                               have passed in a "next" entry even though there wasn't
                               actually a "next". *)
                  val { file, startLine, startPosition, ... } = loc
                  val lastPos =
                      { file = file, startLine = startLine, endLine = startLine,
                        startPosition = startPosition, endPosition = startPosition }
                  val opts =
                      List.filter(fn PTparent _ => true | PTpreviousSibling _ => true | _ => false) opts
                in
                  (lastPos, opts)
                end
             |   SOME (PTnextSibling f) => move(f(), n-1)
             |   SOME _ => raise Match (* Shouldn't happen *)
      in
        case ! parseRootRef of
            [] => raise Fail "Empty Tree"
          | trees => move(makelist(trees, []), n)
      end

    val previous =
        case currentList of
            [] => NONE (* This is the first. *)
         |   _ => SOME(fn () => moveToNth(itemCount-1))
    fun next () = moveToNth(itemCount+1)
  in
    { parent = SOME parent, next = SOME next, previous = previous }
  end

(* **************************************** *)
(* Compile stuff *)
(* FIXME bigtime untangling WIP *)

(* Even success may include warning messages. FIXME surely redundant *)
datatype compile_result
  = Success
  | Exception of exn
  | Interrupted
  | Errors

local

  open PolyML.NameSpace

  (* Put in the results without printing. *)
  fun resultFun
        { fixes: (string * Infixes.fixity) list, values: (string * Values.value) list,
          structures: (string * Structures.structureVal) list, signatures: (string * Signatures.signatureVal) list,
          functors: (string * Functors.functorVal) list, types: (string * TypeConstrs.typeConstr) list} =
    let
      open PolyML
    in
      List.app (#enterFix globalNameSpace) fixes;
      List.app (#enterType globalNameSpace) types;
      List.app (#enterSig globalNameSpace) signatures;
      List.app (#enterStruct globalNameSpace) structures;
      List.app (#enterFunct globalNameSpace) functors;
      List.app (#enterVal globalNameSpace) values
    end

in

(* Compile the prelude.  Simply returns true if it succeeded and false on any error.
   Note: Unlike the main compilation this is run with the interlock held and
   interrupts deferred.
*)
fun compilePreludeString (stringInput : string) : string option =
  let
    val stringStream = TextIO.openString stringInput

    fun compilerResultFun (_, codeOpt) =
      case codeOpt of
          SOME code => (fn () => resultFun(code()))
       |  NONE => raise Fail "Static Errors"

    fun compilerLoop () =
      (* Compile each "program" until either we get to the end or an exception. *)
      if TextIO.endOfStream stringStream
      then NONE (* Reached the end of the input without error. *)
      else
        let
          (* Compile the code and get the result. *)
          open PolyML PolyML.Compiler
          val (code, result) =
              ( PolyML.compiler ( fn () => TextIO.input1 stringStream
                                , [ CPOutStream IO.print (* TextIO.print *)
                                  , CPCompilerResultFun compilerResultFun
                                  ])
              , NONE )
              handle exn => (fn() => (), SOME(exnMessage exn))
        in
          case result of
              NONE =>
              (
                (* No exception in compiler: run the code and check that it
                   runs successfully. *)
                case ((code(); NONE) handle exn => SOME(exnMessage exn)) of
                    NONE => compilerLoop () (* Continue. *)
                  | exn => exn
              )
            | error => error
        end

    fun runloop () =
      let
        val res = compilerLoop ()
      in
        (* The prelude may update the current parse tree. *)
        case !parseTree of
            (_, []) => lastParsetree := NONE
          | (_, hd :: _) => lastParsetree := SOME hd;
        res
      end
  in
    withLock runloop
  end

fun compile_prelude { requestId, fileName, startPosition, preludeCode, sourceCode } =
  let
    fun prelude_error err =
      let
        (* Leave the parse tree unchanged. *)
        val (_, _, currentId) = getCurrentParse()
      in
        CompilerResponse {
          requestId = requestId, parseTreeId = currentId,
          finalOffset = startPosition, result = PreludeFail err
        }
      end
  in
    print ("Compiling prelude: '" ^ preludeCode ^ "'\n")
  ; Option.map prelude_error (compilePreludeString preludeCode)
  end

(* Compile the main source code. *)
fun compileString (fileName, stringInput, startPosition: int) =
  let
    val errorList = ref []
    val stringPosition = ref 0
    val stringSize = String.size stringInput
    val resultTrees : PolyML.parseTree list ref = ref []
    val lastTreePosition = ref 0

    fun readIn () =
      let
        val posn = !stringPosition
      in
        if posn >= stringSize
        then NONE
        else SOME(String.sub(stringInput, posn)) before (stringPosition := posn+1)
      end

    (* We need to define our own compilerResultFun in order to capture the parse trees. *)
    fun compilerResultFun (parsetree, codeOpt) =
      (
        (* Add the parsetree to the list.  Record this as the position of the last valid tree. *)
        case parsetree of
            SOME pt =>
            (resultTrees := ! resultTrees @ [pt]; lastTreePosition := !stringPosition)
          | NONE => (); (* Not if parse failed. *)
        case codeOpt of
            SOME code => (fn () => resultFun(code()))
          | NONE => raise Fail "Static Errors"
      )

    fun compilerLoop () =
      (* Compile each "program" until either we get to the end or an exception. *)
      if ! stringPosition >= stringSize
      then Success (* Reached the end of the input without error. *)
      else
        let
          open PolyML PolyML.Compiler
          val (code, result) =
              (PolyML.compiler ( readIn
                               , [ CPOutStream IO.print(* TextIO.print *)
                                 , CPLineOffset (fn () => startPosition + !stringPosition)
                                 , CPErrorMessageProc (fn msg => errorList := !errorList @ [msg])
                                 , CPCompilerResultFun compilerResultFun
                                 , CPFileName fileName
                                 , CPRootTree (toplevelParseTree resultTrees)
                                 ] )
              , Success)
              handle Fail _ => (fn() => (), Errors)
                   | _ (* E.g. Interrupted *) => (fn() => (), Interrupted)
        in
          case result of
              Success => (* Compilation succeeded. *)
              (
                (* Run the code.  If it raised an exception pass that back. *)
                case (code(); Success) handle exn => Exception exn of
                    Success => compilerLoop () (* Continue. *)
                  | fault => fault
              )
            | error => error
        end
  in
    (compilerLoop (), startPosition + !lastTreePosition, !resultTrees, !errorList)
  end

end

fun compile_main { requestId, fileName, startPosition, preludeCode, sourceCode } : response =
  let
    val () = print ("Compiling main: '" ^ sourceCode ^ "'\n")

    (* The rest of this code is interruptible
       TODO: Multiple interrupts could result in not sending a result packet. *)
    val () = let open (* Thread. *) Thread in
               setAttributes [ EnableBroadcastInterrupt true
                             , InterruptState InterruptAsynch]
             end
    val (result, finalPosition, resultTrees, errors) =
        compileString(fileName, sourceCode, FixedInt.toInt startPosition)

    val () = print ("Done compiling main!\n")

    fun makeErrorPacket {message: PolyML.pretty, hard: bool, location, ...} =
      { hardError = hard, location = location, message = message }
    val errorPackets = List.map makeErrorPacket errors
    val compile_result =
        case result of
            Success => (print "FIXME MAIN COMPILE SUCCESS\n"; Succeeded errorPackets) (* May be warning messages. *)
          | Exception exn =>
            let
              val () = print "FIXME MAIN COMPILE GOT EXN\n"
              open PolyML
              val exLoc =
                  case exceptionLocation exn of
                      SOME loc => [ContextLocation loc]
                    | NONE => []
              val exceptionString =
                  (PrettyBlock(0, false, exLoc,
                               [ prettyRepresentation(exn, FixedInt.fromInt(!PolyML.Compiler.printDepth)) ]))
             in
               RuntimeException(exceptionString, errorPackets)
             end
          | Interrupted => (print "FIXME MAIN COMPILE GOT INTERRUPTED\n"; CompileCancelled errorPackets)
          | Errors => (print "FIXME MAIN COMPILE GOT ERRORS\n"; CompileFail errorPackets)
    (* Update the tree unless parsing failed and we don't have one. *)
    val parseTreeId =
        case resultTrees of
            [] => #3 (getCurrentParse()) (* Return existing tree. *)
          | _ => (setParseTree(resultTrees, requestId); requestId)
    val () = print ("compile_main: returning response\n")
  in
    CompilerResponse
      { requestId = requestId, parseTreeId = parseTreeId
      , finalOffset = FixedInt.fromInt finalPosition, result = compile_result }
  end

fun compile_thread req () : response =
  case compile_prelude req of
      SOME response => response
    | NONE => compile_main req

(* Asynchronously invoke the compiler. *)
(* FIXME actually do it synchronously and wrap it in the threading stuff *)
(* FIXME generalize the state here and get rid of the refs ? *)
fun compile_request (req as {requestId, startPosition, ...}) : process =
  fn currentCompilation =>
     let
       (* First see if the last compilation has terminated.  Starting a new
          compilation before the previous one has finished is really a
          protocol error. FIXME sorta: they could be independent, or queued. Depends on how much isolation we can achieve. *)
       val isStillRunning =
           case currentCompilation of
               NONE => false
             | SOME (_, lastCompileThread) => (* Thread. *)Thread.isActive lastCompileThread
     in
       if isStillRunning
       then
         ( Continue (SOME (CompilerResponse { requestId = requestId, parseTreeId = #3 (getCurrentParse()),
                                              finalOffset = startPosition, result = PreludeFail "Thread still running" }))
         , currentCompilation )
       else
         ( Thread { requestId = requestId
                  , f = compile_thread req }
         , currentCompilation ) (* FIXME this state is useless; rethink the monad abstraction *)
     end

fun kill_request {requestId} : process =
  fn currentCompilation =>
     ( ( case currentCompilation of
             NONE => () (* No compilation. *)
           | SOME (id, thread) =>
             if requestId = id
             then (*Thread.*)Thread.interrupt thread
             else () (* Different ID running. *) )
     ; (Continue NONE, currentCompilation) )

(* **************************************** *)
(* Top level dispatch *)

fun hello_request {requestId} : process =
  StateM.return (Continue (SOME (HelloResponse { requestId = requestId, version = VERSION })))

fun user_request {requestId, payload} : process =
  ( IO.put payload
  ; StateM.return (Continue NONE) )

fun process (req : request) : process =
  case req of
      HelloRequest req => hello_request req
    | CompileRequest req => compile_request req
    | KillRequest req => kill_request req
    | UserRequest req => user_request req
    | _ => raise FIXME "process: unimplemented."

(*

FIXME should the web socket API do the mutex?
Note that close can interfere with send
FIXME parameterize on transport, i.e. JSON fns.

*)

fun handle_polyml_ide (fns : Http_websockets.fns) : unit =
  let
    val out_mutex = (* Thread. *) Mutex.mutex ()
    val close = fn () => () (* ThreadLib.protect out_mutex (#send fns) *)
    val recv = #recv fns
    val send = ThreadLib.protect out_mutex (#send fns)
               o Pair.mapSnd (CoSeq.map Byte.stringToBytes) (* FIXME let's use strings for now, but... change where? *)
    fun aux state =
      case read_request recv of (* FIXME this is where we should do poll loop thing, Async_poll.sock_evt *)
          NONE => close ()
        | SOME request => (* FIXME monadify? TRO? *)
            let
              val (response, state') = process request state
              val state' =
                  case response of
                      Continue resp => ( Option.app (send_response send) resp; state' )
                    | Thread {requestId, f} =>
                      let
                        val thread = Thread.fork ( send_response send o f
                                                 , [(* Thread. *)Thread.InterruptState (* Thread. *)Thread.InterruptDefer] )
                      in
                        SOME (requestId, thread)
                      end
            in
              aux state'
            end
  in
    user_output_send := send;
    aux NONE
  end

(*


                 (* Read the ASN1 header to get the tag and then read the data.
                    Position the stream ready to read the next request. *)
                 val (requestTag, data) =
                     case readHeader BinIO.StreamIO.input1 (!inStream) of
                         NONE => (* If we had EOF here it's probably because we've closed. *)
                             raise Fail "FIXME OS.Process.exit OS.Process.success (* Close down. *)"
                     |   SOME((tag, length), afterHdr) =>
                         let
                             val (vector, afterBlock) =
                                 BinIO.StreamIO.inputN(afterHdr, length)
                         in
                             if Word8Vector.length vector = length
                             then ()
                             else protocolError "Stream closed";
                             inStream := afterBlock;
                             (tag, vector)
                         end

                 fun splitSequence v =
                     case decodeItem v of
                         SOME{tag, data, remainder} =>
                             (tag, data) :: splitSequence remainder
                     |   NONE => []

                 (* See if an item is present and return it if it is. *)
                 fun findData tag list =
                     Option.map #2 (List.find (fn (t, _) => t = tag) list)

                 fun findString tag list =
                     Option.map decodeString (findData tag list)
                 and findInt tag list =
                     Option.map (FixedInt.fromInt o decodeInt) (findData tag list)
             in

                 |   Application(4, _) => (* Return the type of the selected node. *)
                     let
                         val tdList = splitSequence(Word8VectorSlice.full data)
                         (* Request id *)
                         val reqId = findString (Application(1, Primitive)) tdList
                         (* Parse id *)
                         val parseId = findString (Application(2, Primitive)) tdList
                         (* Start offset *)
                         val startOff = findInt (Context(1, Primitive)) tdList
                         (* End offset *)
                         val endOff = findInt (Context(2, Primitive)) tdList
                     in
                         case (reqId, parseId, startOff, endOff) of
                             (SOME requestId, SOME parseTreeId, SOME startOffset, SOME endOffset) =>
                                 TypeRequest{
                                     requestId = requestId, parseTreeId = parseTreeId,
                                     location = { startOffset = startOffset, endOffset = endOffset }
                                     }
                         |   (SOME requestId, _, _, _) => UnknownRequest { request = 4, requestId = requestId }
                         |   _ => UnknownRequest { request = 4, requestId = "" } (* Malformed *)
                     end

                 |   Application(5, _) => (* Move request. *)
                     let
                         val tdList = splitSequence(Word8VectorSlice.full data)
                         (* Request id *)
                         val reqId = findString (Application(1, Primitive)) tdList
                         (* Parse id *)
                         val parseId = findString (Application(2, Primitive)) tdList
                         (* Start offset *)
                         val startOff = findInt (Context(1, Primitive)) tdList
                         (* End offset *)
                         val endOff = findInt (Context(2, Primitive)) tdList
                         (* Move direction *)
                         val dir = findInt (Context(3, Primitive)) tdList
                     in
                         case (reqId, parseId, startOff, endOff, dir) of
                             (SOME requestId, SOME parseTreeId, SOME startOffset,
                              SOME endOffset, SOME dir) =>
                             let
                                 val dirn =
                                     case dir of
                                         1 => DirUp
                                     |   2 => DirLeft
                                     |   3 => DirRight
                                     |   _ (*4*) => DirDown
                             in
                                 MoveRequest{
                                     requestId = requestId, parseTreeId = parseTreeId, direction = dirn,
                                     location = { startOffset = startOffset, endOffset = endOffset }
                                     }
                             end
                         |   (SOME requestId, _, _, _, _) => UnknownRequest { request = 5, requestId = requestId }
                         |   _ => UnknownRequest { request = 5, requestId = "" } (* Malformed *)

                     end

                 |   Application(6, _) => (* Declaration location for variables. *)
                     let
                         val tdList = splitSequence(Word8VectorSlice.full data)
                         (* Request id *)
                         val reqId = findString (Application(1, Primitive)) tdList
                         (* Parse id *)
                         val parseId = findString (Application(2, Primitive)) tdList
                         (* Start offset *)
                         val startOff = findInt (Context(1, Primitive)) tdList
                         (* End offset *)
                         val endOff = findInt (Context(2, Primitive)) tdList
                         (* Dec type *)
                         val dt = findInt (Context(3, Primitive)) tdList
                     in
                         case (reqId, parseId, startOff, endOff, dt) of
                             (SOME requestId, SOME parseTreeId, SOME startOffset,
                              SOME endOffset, SOME dect) =>
                             let
                                 val decType =
                                     case dect of
                                         2 => DecOpen
                                     |   3 => DecParent
                                     |   _ (*1*) => DecLocal
                             in
                                 DecRequest{
                                     requestId = requestId, parseTreeId = parseTreeId, decType = decType,
                                     location = { startOffset = startOffset, endOffset = endOffset }
                                     }
                             end
                         |   (SOME requestId, _, _, _, _) => UnknownRequest { request = 6, requestId = requestId }
                         |   _ => UnknownRequest { request = 6, requestId = "" } (* Malformed *)
                     end

                 |   Application(7, _) => (* List the references to a variable. *)
                     let
                         val tdList = splitSequence(Word8VectorSlice.full data)
                         (* Request id *)
                         val reqId = findString (Application(1, Primitive)) tdList
                         (* Parse id *)
                         val parseId = findString (Application(2, Primitive)) tdList
                         (* Start offset *)
                         val startOff = findInt (Context(1, Primitive)) tdList
                         (* End offset *)
                         val endOff = findInt (Context(2, Primitive)) tdList
                     in
                         case (reqId, parseId, startOff, endOff) of
                             (SOME requestId, SOME parseTreeId, SOME startOffset, SOME endOffset) =>
                                 RefRequest{
                                     requestId = requestId, parseTreeId = parseTreeId,
                                     location = { startOffset = startOffset, endOffset = endOffset }
                                     }
                         |   (SOME requestId, _, _, _) => UnknownRequest { request = 7, requestId = requestId }
                         |   _ => UnknownRequest { request = 7, requestId = "" } (* Malformed *)
                     end

                 |   Universal(tagNo, _) => UnknownRequest { request = tagNo, requestId = "" }
                 |   Application(tagNo, _) => UnknownRequest { request = tagNo, requestId = "" }
                 |   Context(tagNo, _) => UnknownRequest { request = tagNo, requestId = "" }
                 |   Private(tagNo, _) => UnknownRequest { request = tagNo, requestId = "" }
                 (*case startCh of
                 |   #"O" => (* Print list of valid commands. *)
                     let
                         val requestId = readToEscape("", #",")
                         val parseTreeId = readToEscape("", #",")
                         val startOffset = getInt #","
                         val endOffset = getInt #"o"
                     in
                         PropertyRequest{
                             requestId = requestId, parseTreeId = parseTreeId,
                             location = { startOffset = startOffset, endOffset = endOffset }
                             }
                     end

                 |   #"K" => (* Cancel request. *)
                         KillRequest { requestId = readToEscape ("", #"k") }
                 *)
             end (* readRequest *)





             local
                 (* Create a functional binary stream *)
(* FIXME why? Consider an imperative one. *)
                 val reader =
                     BinPrimIO.RD {
                         name = "socket", chunkSize = prefBuffSize,
                         readVec = SOME readVec, readArr = NONE, readVecNB = NONE,
                         readArrNB = NONE, block = NONE,
                         canInput = NONE, avail = fn _ => NONE,
                         getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE,
                         close = fn _ => (), ioDesc = NONE (* FIXME SOME(Socket.ioDesc socket) *)
                     }
                 val binStream =
                     BinIO.StreamIO.mkInstream(BinPrimIO.augmentReader reader, Word8Vector.fromList [])
             in
                 val inStream = ref binStream
             end




    fun ideProtocolV2 ({sendVecList, readVec} : ideProtocolV2Transport) : ideProtocol =
         let
             val prefBuffSize = 4096 (* Get this from somewhere? *)

             open TextIO Asn1

             (*
             (* FIXME we really don't want to do this: the terminal Poly/ML runs in acts as a log.
                The protocol should use sendVecList when it wants to send stuff to the IDE. *)
             local (* Interlocked writer for TextIO.stdOut *)
                 (* Whenever we write plain text we package it as an ASN1 packet. *)
                 fun writeVecToSocket(v: CharVectorSlice.slice) =
                     (
                         sendVecList(encodeItem(Application(1, Primitive), [encodeString(CharVectorSlice.vector v)]));
                         CharVectorSlice.length v (* It's written it all. *)
                     )
                 val lockedWriteVec = ThreadLib.protect outputLock writeVecToSocket
                 val lockedWriter =
                     TextPrimIO.WR {
                         name = "TextIO.stdOut", chunkSize = prefBuffSize,
                         writeVec = SOME lockedWriteVec, writeArr = NONE,
                         writeVecNB = NONE, writeArrNB = NONE, block = NONE, canOutput = NONE,
                         getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE,
                         close = fn () => raise Fail "stdOut must not be closed",
                         ioDesc = NONE (* FIXME SOME(Socket.ioDesc socket) *) }
             in
                 (* Use this locked version for normal stdOut. *)
                 val () = setOutstream(stdOut,
                             StreamIO.mkOutstream(TextPrimIO.augmentWriter lockedWriter, IO.LINE_BUF))
             end
             *)

             local
                 (* Create a functional binary stream *)
                 val reader =
                     BinPrimIO.RD {
                         name = "socket", chunkSize = prefBuffSize,
                         readVec = SOME readVec, readArr = NONE, readVecNB = NONE,
                         readArrNB = NONE, block = NONE,
                         canInput = NONE, avail = fn _ => NONE,
                         getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE,
                         close = fn _ => (), ioDesc = NONE (* FIXME SOME(Socket.ioDesc socket) *)
                     }
                 val binStream =
                     BinIO.StreamIO.mkInstream(BinPrimIO.augmentReader reader, Word8Vector.fromList [])
             in
                 val inStream = ref binStream
             end

             fun protocolError error =
             let
                 open OS.Process
             in
                 TextIO.print ("Protocol error: " ^ error) handle _ => ();
                 exit failure
             end


             fun sendStartedMessage () =
             let
                 fun sendResponse () =
                     sendVecList(encodeItem(Application(2, Primitive), [encodeString "1.0.0"]))
             in
               print ">> SEND STARTED MESSAGE <<\n";
                 sendResponse ();
                 (* ThreadLib.protect outputLock sendResponse (); *)
               print ">> SEND STARTED MESSAGE DONE <<\n"
             end

             (* Send a reply packet. *)
             fun sendResponse response =
             let
                 fun encodeFullLocation { file, startLine, startPosition, endPosition, ...} =
                 let
                     val encFile =
                         if file = "" then [] else encodeItem(Context(1, Primitive), [encodeString file])
                     val encLine =
                         if startLine = 0 then [] else encodeItem(Context(2, Primitive), [encodeInt(FixedInt.toInt startLine)])
                     val encStart =
                         if startPosition = 0 then [] else encodeItem(Context(3, Primitive), [encodeInt(FixedInt.toInt startPosition)])
                     val encEnd =
                         if endPosition = 0 then [] else encodeItem(Context(4, Primitive), [encodeInt(FixedInt.toInt endPosition)])
                 in
                     encFile @ encLine @ encStart @ encEnd
                 end

                 and encodeLocation {startOffset, endOffset} =
                         encodeItem(Context(3, Primitive), [encodeInt(FixedInt.toInt startOffset)]) @
                         encodeItem(Context(4, Primitive), [encodeInt(FixedInt.toInt endOffset)])

                 and encodeRequestId requestId =
                     encodeItem(Application(20, Primitive), [encodeString requestId])

                 and encodeParseId parseId =
                     encodeItem(Application(21, Primitive), [encodeString parseId])

                 fun mapEnc _ [] = []
                 |   mapEnc f (hd :: tl) = f hd @ mapEnc f tl

                 (* Turn a pretty-print structure into text, stripping out mark-up. *)
                 (* TODO: We could return the "pretty" structure and have the IDE format it. *)
                 fun prettyAsString message =
                 let
                     val result = ref []
                     fun doPrint s = result := s :: ! result
                     val () = PolyML.prettyPrint(doPrint, 120(*!PolyML.Compiler.lineLength*)) message
                 in
                     String.concat(List.rev(! result))
                 end

                 fun makeResponse (CompilerResponse { requestId, parseTreeId, finalOffset, result }) =
                     let
                         fun encodeError { hardError, location, message } =
                             encodeItem(Context(4, Constructed),
                                 encodeItem(Context(1, Primitive), [encodeBool hardError]) @
                                 encodeItem(Context(3, Constructed), encodeFullLocation location) @
                                 encodeItem(Context(2, Primitive), [encodeString(prettyAsString message)])
                                 )

                         val (resultCode, resultData) =
                             case result of
                                 Succeeded errors =>
                                     (0, mapEnc encodeError errors)
                             |   RuntimeException (s, errors) =>
                                     (1, encodeItem(Context(3, Primitive),
                                         [encodeString(prettyAsString s)]) @ mapEnc encodeError errors)
                             |   PreludeFail s =>
                                     (2, encodeItem(Context(3, Primitive), [encodeString s]))
                             |   CompileFail errors =>
                                     (3, mapEnc encodeError errors)
                             |   CompileCancelled errors =>
                                     (4, mapEnc encodeError errors)
                     in
                         sendVecList(encodeItem(Application(3, Constructed),
                             encodeRequestId requestId @ encodeParseId parseTreeId @
                             encodeItem(Context(1, Primitive), [encodeInt(FixedInt.toInt finalOffset)]) @
                             encodeItem(Context(2, Primitive), [encodeInt(FixedInt.toInt resultCode)]) @
                             resultData))
                     end

                 |   makeResponse (PropertyResponse { requestId, parseTreeId, location, commands }) =
                     let
                         fun encCommand c = encodeItem(Context(2, Primitive), [encodeString c])
                     in
                         sendVecList(encodeItem(Application(4, Constructed),
                             encodeRequestId requestId @ encodeParseId parseTreeId @
                             encodeItem(Context(1, Constructed), encodeLocation location) @
                             mapEnc encCommand commands))
                     end

                 |   makeResponse (MoveResponse { requestId, parseTreeId, location }) =
                         sendVecList(encodeItem(Application(7, Constructed),
                             encodeRequestId requestId @ encodeParseId parseTreeId @
                             encodeItem(Context(1, Constructed), encodeLocation location)))

                 |   makeResponse (TypeResponse { requestId, parseTreeId, location, typeRes }) =
                     let
                         val typeData =
                             case typeRes of
                                 NONE => []
                             |   SOME t => encodeItem(Context(2, Primitive), [encodeString(prettyAsString t)])
                     in
                         sendVecList(encodeItem(Application(8, Constructed),
                             encodeRequestId requestId @ encodeParseId parseTreeId @
                             encodeItem(Context(1, Constructed), encodeLocation location) @ typeData))
                     end

                 |   makeResponse (DecResponse { requestId, parseTreeId, location, decLocation }) =
                     let
                         val decData =
                             case decLocation of
                                 NONE => []
                             |   SOME l => encodeItem(Context(2, Constructed), encodeFullLocation l)
                     in
                         sendVecList(encodeItem(Application(9, Constructed),
                             encodeRequestId requestId @ encodeParseId parseTreeId @
                             encodeItem(Context(1, Constructed), encodeLocation location) @ decData))
                     end

                 |   makeResponse (RefResponse { requestId, parseTreeId, location, references }) =
                     let
                         fun encLoc l = encodeItem(Context(2, Constructed), encodeLocation l)
                     in
                         sendVecList(encodeItem(Application(10, Constructed),
                             encodeRequestId requestId  @ encodeParseId parseTreeId @
                             encodeItem(Context(1, Constructed), encodeLocation location) @
                             mapEnc encLoc references))
                     end

                 |   makeResponse (UnknownResponse { requestId, ... }) =
                         (* Send an Error packet. *)
                         sendVecList(encodeItem(Application(0, Constructed),
                             if requestId = "" then []
                             else encodeRequestId requestId))

                 fun sendResponse () =
                 (
                     makeResponse response handle _ => protocolError "Exception"
                 )
             in
                 (* Sending the response packet must be atomic with respect to any other
                    output to stdOut. *)
                 ThreadLib.protect outputLock sendResponse ()
             end (* sendResponse *)
         in
             {readRequest=readRequest, sendStartedMessage=sendStartedMessage, sendResponse=sendResponse}
         end

    fun runIDEWithProtocol ({readRequest, sendStartedMessage, sendResponse} : ideProtocol) =
    let


        (* Move in the selected direction.  Returns the tree as the result of the move. *)
        fun navigateTo(searchLocation as {startOffset:FixedInt.int, endOffset:FixedInt.int}, lastParsetree) =
        case lastParsetree of
            NONE => NONE
        |   SOME({ startPosition, endPosition, ... }, tree) =>
            let
                open PolyML
                datatype direction = Up | Down | Left | Right
                fun find([], _) = NONE (* No change *)
                |   find(PTparent p :: _, Up) = SOME p
                |   find(PTpreviousSibling p :: _, Left) = SOME p
                |   find(PTnextSibling p :: _, Right) = SOME p
                |   find(PTfirstChild p :: _, Down) = SOME p
                |   find(_ :: tl, dir) = find (tl, dir)
            in
                if startOffset = startPosition andalso endOffset = endPosition
                then (* We're there already. *) lastParsetree
                else if startOffset >= startPosition andalso endOffset <= endPosition
                then (* It's this node or a child. *)
                    let
                        val child = find(tree, Down)
                    in
                        (* See if the element we want is actually a child. *)
                        case child of
                            SOME child =>
                            let
                                (* See which child it is. *)
                                fun findChild(location as {startPosition, endPosition, ...}, child) =
                                    if startOffset >= startPosition andalso endOffset <= endPosition
                                    then SOME (location, child)
                                    else
                                    case find(child, Right) of
                                        NONE => NONE
                                     |   SOME next => findChild(next())
                            in
                                case findChild(child()) of
                                    NONE => lastParsetree (* In this *)
                                |   SOME child => navigateTo(searchLocation, SOME child)
                            end
                        |   NONE => lastParsetree (* No children. *)
                    end
                else (* Must go out. *)
                (
                    case find(tree, Up) of
                        SOME p => navigateTo(searchLocation, SOME(p()))
                    |   NONE => NONE (* Not found *)
                )
            end

        (* Main protocol loop. *)
        fun runProtocol currentCompilation =
        let
            (* Return the location of the given tree. *)
            fun treeLocation NONE = {startOffset = 0, endOffset = 0}
            |   treeLocation (SOME ({startPosition, endPosition, ...}, _)) =
                        {startOffset = startPosition, endOffset = endPosition}
        in
            case readRequest () of
                PropertyRequest { requestId: string, parseTreeId: string, location } =>
                let (* Properties of selected node. *)
                    (* Get the current parse tree and check the ID matches *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (commands, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            val newTree = navigateTo(location, lastParsetree)
                            (* Update the last tree if it's still valid. *)
                            val () = updateLastParse(currentParseID, newTree)
                            val commands =
                                case newTree of
                                    NONE => []
                                |   (SOME(_, tree)) =>
                                    let
                                        open PolyML
                                        fun printCode(PTparent _, rest) = "U" :: rest
                                        |   printCode(PTpreviousSibling _, rest) = "P" :: rest
                                        |   printCode(PTnextSibling _, rest) = "N" :: rest
                                        |   printCode(PTfirstChild _, rest) = "C" :: rest
                                        |   printCode(PTtype _, rest) = "T" :: rest
                                        |   printCode(PTdeclaredAt _, rest) = "I" :: rest
                                        |   printCode(PTopenedAt _, rest) = "J" :: rest
                                        |   printCode(PTstructureAt _, rest) = "S" :: rest
                                        |   printCode(PTreferences(_, _::_), rest) = "V" :: rest
                                                (* Only include references if there is at least one
                                                   local reference. *)
                                        |   printCode(PTreferences(_, []), rest) = rest
                                        |   printCode(PTprint _, rest) = rest
                                        |   printCode(PTbreakPoint _, rest) = rest
                                        |   printCode(PTcompletions _, rest) = rest
                                        |   printCode(PTdefId _, rest) = rest
                                        |   printCode(PTrefId _, rest) = rest
                                    in
                                        List.foldl printCode [] tree
                                    end
                        in
                            (commands, treeLocation newTree)
                        end
                        else ([], { startOffset = 0, endOffset = 0 }) (* Wrong ID. *)
                in
                    sendResponse(
                        PropertyResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, commands = commands
                        });
                    runProtocol currentCompilation
                end

            |   MoveRequest { requestId, parseTreeId, location, direction } =>
                let  (* Get location after a move relative to a selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val newLocation =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the given location, then move in the required direction. *)
                            val newTree =
                                case navigateTo(location, lastParsetree) of
                                    NONE => NONE
                                |   SOME(location, tree) =>
                                    let
                                        open PolyML
                                        fun find([], _) = (location, tree) (* No change *)
                                        |   find(PTparent p :: _, DirUp) = p()
                                        |   find(PTpreviousSibling p :: _, DirLeft) = p()
                                        |   find(PTnextSibling p :: _, DirRight) = p()
                                        |   find(PTfirstChild p :: _, DirDown) = p()
                                        |   find(_ :: tl, dir) = find (tl, dir)

                                    in
                                        SOME(find(tree, direction))
                                    end
                            (* Update the last tree if it's still valid. *)
                            val () = updateLastParse(currentParseID, newTree)
                        in
                            treeLocation newTree (* Return the location of the updated tree. *)
                        end
                        else { startOffset = 0, endOffset = 0 } (* *)
                in
                    sendResponse(
                        MoveResponse {
                            requestId = requestId, parseTreeId = currentParseID, location = newLocation
                        });
                    runProtocol currentCompilation
                end

            |   TypeRequest { requestId, parseTreeId, location } =>
                let (* Type of value at selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (typeRes, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the required location. *)
                            val newTree = navigateTo(location, lastParsetree)
                            val () = updateLastParse(currentParseID, newTree)
                            (* If it has a type return it. *)
                            val typeRes =
                                case newTree of
                                    NONE => NONE
                                |   (SOME(_, tree)) =>
                                    (
                                        (* Print the type if it's there.  Don't include any mark-up. *)
                                        (* TODO: This uses the global name space to find types and structures.
                                           It really should use the local name space but that requires adding
                                           an environment to the parse tree. *)
                                        case List.find (fn (PolyML.PTtype _) => true | _ => false) tree of
                                            SOME(PolyML.PTtype t) =>
                                                SOME(PolyML.NameSpace.Values.printType(t, 100, SOME PolyML.globalNameSpace))
                                        |   _ => NONE
                                    )
                        in
                           (typeRes, treeLocation newTree)
                        end
                        else (NONE, { startOffset = 0, endOffset = 0 })
                in
                    sendResponse(
                        TypeResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, typeRes = typeRes
                        });
                    runProtocol currentCompilation
                end

            |   DecRequest { requestId, parseTreeId, location, decType } =>
                let (* Information about declaration location of identifier at selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (decLocation, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the required location. *)
                            val newTree = navigateTo(location, lastParsetree)
                            val () = updateLastParse(currentParseID, newTree)
                            val decLocation =
                                (* If it has the right kind of property return it. *)
                                case newTree of
                                    NONE => NONE
                                |   (SOME(_, tree)) =>
                                    let
                                        open PolyML
                                        val getLoc =
                                            case decType of
                                                DecLocal => (fn (PTdeclaredAt p) => SOME p | _ => NONE)
                                            |   DecOpen => (fn (PTopenedAt p) => SOME p | _ => NONE)
                                            |   DecParent => (fn (PTstructureAt p) => SOME p | _ => NONE)
                                        (* Seatch in the properties of the current node for the property we want. *)
                                        fun findLoc [] = NONE
                                        |   findLoc (hd::tl) =
                                            case getLoc hd of
                                                SOME location => SOME location
                                            |   NONE => (* Keep trying. *) findLoc tl
                                    in
                                        findLoc tree
                                    end
                        in
                            (decLocation, treeLocation newTree)
                        end
                        else (NONE, { startOffset = 0, endOffset = 0 })
                in
                    sendResponse(
                        DecResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, decLocation = decLocation
                        });
                    runProtocol currentCompilation
                end

            |   RefRequest { requestId, parseTreeId, location } =>
                let (* Type of value at selected node. *)
                    val (_, lastParsetree, currentParseID) = getCurrentParse()
                    val (references, location) =
                        if parseTreeId = currentParseID
                        then
                        let
                            (* Move to the required location. *)
                            val newTree = navigateTo(location, lastParsetree)
                            val () = updateLastParse(currentParseID, newTree)
                            (* Find the local references. *)
                            val references =
                                case newTree of
                                    NONE => []
                                |   SOME(_, tree) =>
                                    (
                                        case List.find (fn (PolyML.PTreferences _) => true | _ => false) tree of
                                            SOME(PolyML.PTreferences(_, l)) =>
                                                List.map (fn {startPosition, endPosition, ...} =>
                                                            { startOffset=startPosition, endOffset=endPosition}) l
                                        |   _ => []
                                    )
                        in
                           (references, treeLocation newTree)
                        end
                        else ([], { startOffset = 0, endOffset = 0 })
                in
                    sendResponse(
                        RefResponse {
                            requestId = requestId, parseTreeId = currentParseID,
                            location = location, references = references
                        });
                    runProtocol currentCompilation
                end



            |   UnknownRequest req => (* Respond with an empty response. *)
                (
                    sendResponse(UnknownResponse req);
                    runProtocol currentCompilation
                )
        end
    in
        let
            (* Turn off interrupts for the interface thread. *)
            open Thread.Thread
        in
            setAttributes[EnableBroadcastInterrupt false, InterruptState InterruptDefer]
        end;
        sendStartedMessage();
        runProtocol NONE (* No compilation. *)
    end (* runIDEWithProtocol. *)

    fun runIDEProtocol () =
      runIDEWithProtocol
        (case OS.Process.getEnv "POLYIDESOCKET" of
             NONE => ideProtocolV1
           | SOME portNo => ideProtocolV2 (ideProtocolV2Socket portNo))

*)

end
