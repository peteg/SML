(*

A simple HTTP server.

FIXME RFC 2616 is HTTP/1.1
https://www.ietf.org/rfc/rfc2616.txt
http://www.w3.org/Protocols/rfc2616/rfc2616.html

We're supposed to use:
https://www.mnot.net/blog/2014/06/07/rfc2616_is_dead
http://tools.ietf.org/html/rfc7230

which leads to a current (?) grammar:
http://greenbytes.de/tech/webdav/httpbis.abnf

For now, parse just what we need, robustly. Skip the dodgy parts of the HTTP spec.

FIXME futures don't feel quite right: the dispatcher doesn't care
about hearing back. The thread pool sounds useful though. Need to look
at what Makarius did.

FIXME licence on the RFCs.
FIXME consider DOS possibilities.
FIXME timeout: the SML basis docs suggest using select.

*)

(*

3.5.  Message Parsing Robustness:

   Although the line terminator for the start-line and header fields
   is the sequence CRLF, a recipient MAY recognize a single LF as a
   line terminator and ignore any preceding CR.

3.1.1.  Request Line

   HTTP does not place a predefined limit on the length of a
   request-line, as described in Section 2.5.  A server that receives a
   method longer than any that it implements SHOULD respond with a 501
   (Not Implemented) status code.  A server that receives a
   request-target longer than any URI it wishes to parse MUST respond
   with a 414 (URI Too Long) status code (see Section 6.5.12 of
   [RFC7231]).

   Various ad hoc limitations on request-line length are found in
   practice.  It is RECOMMENDED that all HTTP senders and recipients
   support, at a minimum, request-line lengths of 8000 octets.

3.2.5.  Field Limits

   HTTP does not place a predefined limit on the length of each header
   field or on the length of the header section as a whole, as described
   in Section 2.5.  Various ad hoc limitations on individual header
   field length are found in practice, often depending on the specific
   field semantics.

   A server that receives a request header field, or set of fields,
   larger than it wishes to process MUST respond with an appropriate 4xx
   (Client Error) status code.  Ignoring such header fields would
   increase the server's vulnerability to request smuggling attacks
   (Section 9.5).

   A client MAY discard or truncate received header fields that are
   larger than the client wishes to process if the field semantics are
   such that the dropped value(s) can be safely ignored without changing
   the message framing or response semantics.

*)

(* FIXME explain the handler setup. No need for a default? *)
(* FIXME provide ways to register handlers based on methods, paths, etc. *)
(* FIXME settle on sockets or BinIO streams or ... *)

signature HTTP_SERVER =
sig
  type socket = Socket.active INetSock.stream_sock (* FIXME abstract *)

  type configuration
       = { maximum_http_header_size : int (* FIXME in bytes, entire header including request line *)
         , maximum_http_header_time : Time.time (* FIXME time to wait for each byte of the headers to turn up *)
         }

  type request = {req: Request.t, path: Uri.path} (* FIXME remainder of the path after matching prefix *)
  type response = Response.t * string CoSeq.t (* FIXME more explicitly word8 vector/array/whatever? *)
  type request_response = request -> response

  datatype handler_proc
    = Handler of request_response
    | HandlerWithStream of BinIO.instream * BinIO.outstream * request -> unit

  type handler = {method: Method.t, path: Uri.path} * handler_proc

  val change_configuration : (configuration -> configuration) -> unit (* FIXME should be a central mechanism? *)
  val add_handler : handler -> unit
  val respond : BinIO.outstream -> response -> unit
  val handle_http_request : socket -> unit
end

structure Http_server :> HTTP_SERVER =
struct

structure Synchronized = Platform.Synchronized

type socket = Socket.active INetSock.stream_sock (* FIXME abstract *)

(* open Http FIXME *)

(* Configuration. *)

type configuration
     = { maximum_http_header_size : int (* FIXME in bytes, entire header including request line *)
       , maximum_http_header_time : Time.time (* FIXME time to wait for each byte of the headers to turn up *)
       }

val default_configuration : configuration =
    { maximum_http_header_size = 64 * 1024 (* FIXME magic *)
    , maximum_http_header_time = Time.fromSeconds 2 (* FIXME magic *)
    }

val configuration : configuration Synchronized.t =
    Synchronized.var "Http_server.configuration" default_configuration

val change_configuration = Synchronized.change configuration
fun get_configuration () = Synchronized.change_result configuration (fn c => (c, c))

(* Handlers. *)

type request = {req: Request.t, path: Uri.path}
type response = Response.t * string CoSeq.t
type request_response = request -> response

datatype handler_proc
  = Handler of request -> response
  | HandlerWithStream of BinIO.instream * BinIO.outstream * request -> unit

type handler = {method: Method.t, path: Uri.path} * handler_proc

structure MethodMap = AList(Method)

val handlers : handler_proc StringListTrie.t MethodMap.t Synchronized.t =
    Synchronized.var "Http_server.handlers" MethodMap.empty

fun add_handler ({method, path}, cb) : unit =
  let
    val rt_update =
        StringListTrie.insert #1 (path, cb) o Option.get_opt StringListTrie.empty
    val update = MethodMap.insert rt_update method
  in
    Synchronized.change handlers update
  end

local open OptM in
fun dispatch_handler {method, path} : (handler_proc * Uri.path) option =
  (
    Log.log {name="Http_server.dispatch_handler", level=Log.Debug}
            (fn () => "dispatch_handler: " ^ Method.to_string method ^ " " ^ String.concatWith "/" path);
    MethodMap.lookup (Synchronized.value handlers) method
                     >>= (fn rt => StringListTrie.lookup rt path)
  )
end

(* Read the HTTP headers from the socket, don't parse. *)
(* FIXME is input1 inefficient? If so, why? Why can't BinIO buffer for me? *)
(* FIXME timeout *)
(* FIXME representation. char list, string, ?? *)

exception No_headers
exception Headers_too_large

datatype state = CHAR | CR | CRLF | CRLFCR (* Stop after CR LF CR LF *)

fun read_http_headers (instream : BinIO.instream) : string =
    let
      val result = String.implode o rev
      val config = get_configuration ()
      fun drain_leading_whitespace () : char = (* FIXME RFC cite *)
          case BinIO.input1 instream of (* FIXME timeout: socket option somewhere? *)
              NONE => raise No_headers
            | SOME b => let val c = Byte.byteToChar b
                       in if List.mem [#" ", #"\t", #"\n", #"\r"] c (* FIXME etc *)
                          then drain_leading_whitespace ()
                          else c
                       end
      fun read (sz, cs, st) =
          if sz > #maximum_http_header_size config
          then raise Headers_too_large
          else case BinIO.input1 instream of (* FIXME timeout *)
                   NONE => if List.null cs then raise No_headers else String.implode (List.rev cs)
                 | SOME b =>
                   case (Byte.byteToChar b, st) of
                       (#"\r", CHAR)   => read (sz + 1, #"\r" :: cs, CR)
                     | (#"\r", CRLF)   => read (sz + 1, #"\r" :: cs, CRLFCR)
                     | (#"\n", CR)     => read (sz + 1, #"\n" :: cs, CRLF)
                     | (#"\n", CRLFCR) => result (#"\n" :: cs) (* FIXME could reasonably drop the CR LF CR instead. *)
                     | (c, _)          => read (sz + 1, c :: cs, CHAR)
      val init_char = drain_leading_whitespace ()
    in
      read (0, [init_char], CHAR)
    end

(*
   FIXME suck on a CoSeq. Accumulate up to 16k or something. If we get
   that or less, issue a size header. Otherwise use chunked. Idea is
   to try to handle concurrency on the filesystem side...

   See Detritus.chunk. Perhaps use Buffer.t.

   Also perhaps want to compress some things here, e.g. generated SVGs.
   Perhaps not such a big deal on local connections.
*)
(* FIXME exception handling *)
fun respond bout (http_response, data) : unit =
  let
    val () = Response.write_to_outstream bout http_response
    val () = CoSeq.app (fn str => BinIO.output (bout, Byte.stringToBytes str)) data
    val () = BinIO.flushOut bout
  in
    ()
  end

(*
   Note that the /path/ passed to the handler has had the path to the
   handler removed, whereas the full path is in /req/. FIXME it might
   make sense to provide also the path to the handler.
*)
fun handle_http_request (s : Socket.active INetSock.stream_sock) : unit =
  let
    fun go (bin, bout) =
      let
        val name = "Http_server.handle_http_request"
        val http_headers = read_http_headers bin
        val () = Log.log {name=name, level=Log.Debug}
                         (fn () => "HTTP headers:\n-----\n" ^ http_headers ^ "------")
        val http_message = ScanExt.run_parser Request.http_messageP http_headers
      in case http_message of
             NONE => Log.log {name=name, level=Log.Error}
                            (fn () => "FIXME FAILED TO PARSE HTTP MESSAGE")
           | SOME req =>
             (
               Log.log {name=name, level=Log.Debug}
                       (fn () => "Parsed headers:\n----\n" ^ Request.to_string_debug req ^ "----");
               let
                 val rl : Request.request_line = #request_line req
                 val () = case dispatch_handler {method = #method rl, path = #path (Uri.curi_to_uri (#uri rl))} of
                              NONE => Log.log {name=name, level=Log.App}
                                             (fn () => "no handler for request: " ^ Request.to_string req)
                            | SOME (Handler handler, path) => respond bout (handler {req=req, path=path})
                            | SOME (HandlerWithStream handler, path) => handler (bin, bout, {req=req, path=path})
               in
                 Log.log {name=name, level=Log.Debug}
                         (fn () => "Done.")
               end
             )
      end

    fun cleanup (bin, bout) =
      ( (* order is critical *)
        (* FIXME if bout is still open then issue a 501? *)
        (* FIXME MLton chokes on these. When should we close the streams? *)
      (*   BinIO.closeOut bout *)
      (* ; BinIO.closeIn bin *)
      )
  in
    unwind cleanup (Socket_IO.make_streams s) go
  end

end
