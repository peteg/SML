(*

Web sockets.

RFC 6455
https://tools.ietf.org/html/rfc6455

Streamlined RFC:
https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API/Writing_WebSocket_servers

Client side:
https://developer.mozilla.org/en/docs/Web/API/WebSocket

FIXME is this an abstraction violation? Should this not be considered a HTTP handler?
FIXME case insensitivity for the header keys? supposedly yes: so map them all to lower case while reading?

FIXME BinIO is too limited: we receive both binary and text packets on the same connection.

FIXME send PINGs every so often?

Any useful websocket extensions? permessage-deflate

S 3: ws:// URIs do not have fragments
p 17: check origin header? Don't care too much.
FIXME text data is utf8 encoded.

*)

signature HTTP_WEBSOCKETS =
sig

  (* Client error: log, close connection and get out of here. *)
  exception WebSocket of string

  datatype packet_type = EOF | BIN | TXT

  val packet_type_to_string : packet_type -> string

  type fns =
       { recv : unit -> packet_type * Word8Vector.vector
       , send : packet_type * Word8Vector.vector CoSeq.t -> unit
       }

  type handler = fns -> unit

  val handle_websocket : {method: Method.t, path: Uri.path} -> handler -> Http_server.handler
  val handle_echo : handler

end

structure Http_websockets :> HTTP_WEBSOCKETS =
struct

val log_error = Log.log { name = "websocket", level = Log.Error }
val log_info = Log.log { name = "websocket", level = Log.Info }
val log_debug = Log.log { name = "websocket", level = Log.Debug }

exception WebSocket of string

datatype packet_type = EOF | BIN | TXT

val packet_type_to_string : packet_type -> string =
 fn EOF => "EOF"
  | BIN => "BIN"
  | TXT => "TXT"

type fns =
     { recv : unit -> packet_type * Word8Vector.vector
     , send : packet_type * Word8Vector.vector CoSeq.t -> unit
     }

type handler = fns -> unit

val websocket_magic_string = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

(* FIXME maybe work on word8vector in SHA1 too *)
fun websocket_accept_val key =
  Base64.encode (SHA1.rep (SHA1.digest (key ^ websocket_magic_string)))

(* FIXME testing, from the RFC https://tools.ietf.org/html/rfc6455
  val key = "dGhlIHNhbXBsZSBub25jZQ=="
  val () = print ("string_to_hex: " ^ string_to_hex key ^ "\n")
  val () = print ("websocket_accept_val concat: " ^ key ^ websocket_magic_string ^ "\n")
  val () = print ("websocket_accept_val sha1: " ^ string_to_hex (SHA1.rep (SHA1.digest (key ^ websocket_magic_string))) ^ "\n")
  val () = print ("websocket_accept_val: " ^ websocket_accept_val key ^ "\n")

websocket_accept_val concat: dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11
websocket_accept_val sha1: B37A4F2CC0624F1690F64606CF385945B2BEC4EA
websocket_accept_val: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=

*)

(* RFC Sec 5.2. Base Framing Protocol *)
datatype opcode
  = CONTINUATION
  | TEXT
  | BINARY
  | CONNECTION_CLOSE
  | PING
  | PONG

fun to_opcode (b : Word8.word) : opcode =
  case Word8.andb(b, 0wxF) of
      0wx0 => CONTINUATION
    | 0wx1 => TEXT
    | 0wx2 => BINARY
    | 0wx8 => CONNECTION_CLOSE
    | 0wx9 => PING
    | 0wxA => PONG
    | _ => raise WebSocket ("to_opcode: invalid opcode in first byte of packet: '"
                           ^ StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX (Word8.toInt b)))

val from_opcode : opcode -> Word8.word =
 fn CONTINUATION => 0wx0
  | TEXT => 0wx1
  | BINARY => 0wx2
  | CONNECTION_CLOSE => 0wx8
  | PING => 0wx9
  | PONG => 0wxA

val opcode_to_string : opcode -> string =
 fn CONTINUATION => "CONTINUATION"
  | TEXT => "TEXT"
  | BINARY => "BINARY"
  | CONNECTION_CLOSE => "CONNECTION_CLOSE"
  | PING => "PING"
  | PONG => "PONG"

val empty_packet = Word8Vector.fromList []

val packet_type_to_opcode : packet_type -> opcode =
 fn EOF => CONNECTION_CLOSE
  | BIN => BINARY
  | TXT => TEXT

fun send_packet bout (opcode, payload : Word8Vector.vector CoSeq.t) : unit =
  let
    (* FIXME later fragment along CoSeq lines? *)
    val payload = Word8Vector.concat (List.rev (CoSeq.fold (op ::, [], payload)))

    fun fin (b : Word8.word) : Word8.word = Word8.orb(b, 0wx80) (* FIXME no support for fragmentation *)
    fun mask (b : Word8.word) : Word8.word = b (* FIXME no support for masking *)

    val packet0 = fin (from_opcode opcode)
    val (packet1, packets2) : Word8.word * Word8Vector.vector =
        let (* Simpler than the PackedWord shenanigans. Big endian. *)
          val len = Word8Vector.length payload
          val op >> = LargeWord.>>
          fun f w i = Word8.fromLargeWord (LargeWord.fromInt len >> (Word.fromInt (w - i - 1) * 0w8))
        in
          if len >= 65536 (* 2^16; FIXME validate *)
          then
            ( 0w127, Word8Vector.tabulate (8, f 8) )
          else if len >= 126 (* FIXME verify: unsigned int *)
          then
            ( 0w126, Word8Vector.tabulate (2, f 2) )
          else
            ( Word8.fromInt len, empty_packet )
        end
    val packet1 = mask packet1
  in
    BinIO.output1 (bout, packet0);
    BinIO.output1 (bout, packet1);
    BinIO.output (bout, packets2);
    BinIO.output (bout, payload);
    BinIO.flushOut bout
  end

fun ping bout payload =
  send_packet bout (PONG, CoSeq.return payload)

fun pong _ (* bout *) _ (* payload *) = ()

(* RFC 5.5.1.  FIXME Close: server can close TCP connection after sending close? No need to wait for client's "close" response packet? *)
fun connection_close (bin, bout) : packet_type * Word8Vector.vector =
  ( send_packet bout (CONNECTION_CLOSE, CoSeq.return empty_packet)
  ; BinIO.closeOut bout
  ; BinIO.closeIn bin
  ; (EOF, empty_packet)
  )

(* FIXME perhaps use CoSeq here for CONTINUATION data *)
(* RFC p33: control packets may be injected between fragmented packets.
   Deal with ping/pong autonomously here. *)
fun receive_packets (bin, bout) () : packet_type * Word8Vector.vector =
  let
    fun fin (b : Word8.word) : bool = Word8.andb(b, 0wx80) <> 0w0

    fun mask_and_payload_len (bin : BinIO.instream) (b : Word8.word) : bool * int =
      ( Word8.andb(b, 0wx80) <> 0w0
      , case Word8.andb(b, 0wx7F) of
            0w126 => (* payload_len step 2: read next 16 bits as an unsigned big-endian int *)
              let
                val len = BinIO.inputN (bin, PackWord16Big.bytesPerElem)
              in
                Int.fromLarge (LargeWord.toLargeIntX (PackWord16Big.subVecX (len, 0)))
              end
          | 0w127 => raise FIXME "payload_len step 3: read next 64 bits as unsigned int, MSB = 0"
          | n => Word8.toInt n
      )

    fun input_packets01 () : (Word8.word * Word8.word) option =
      let
        val op >>= = OptM.>>=
      in
        BinIO.input1 bin >>= (fn p0 => BinIO.input1 bin >>= (fn p1 => SOME (p0, p1)))
      end

    fun p (packet_type : packet_type option) (payloads : Word8Vector.vector list) =
      case input_packets01 () of
          NONE => ( log_error (fn _ => "FIXME websocket receive_packets: couldn't read header")
                 ; connection_close (bin, bout) )
        | SOME (packet0, packet1) =>
          let
            val fin = fin packet0
            val opcode : opcode = to_opcode packet0
            val (mask, payload_len) = mask_and_payload_len bin packet1

            val () = log_debug (fn _ =>
                                   "handle_websocket: got packet with header:\n\
                              \        FIN: " ^ Bool.to_string fin ^ "\n\
                              \     OPCODE: " ^ opcode_to_string opcode ^ "\n\
                              \       MASK: " ^ Bool.to_string mask ^ "\n\
                              \PAYLOAD LEN: " ^ Int.to_string payload_len)

            val () = if mask then () else raise WebSocket "Client data must be masked."

            val mask = BinIO.inputN (bin, 4)
            val masked_payload = BinIO.inputN (bin, payload_len)
            val payload = Word8Vector.mapi (fn (i, b) => Word8.xorb (b, Word8Vector.sub (mask, i mod 4))) masked_payload

            val () = log_debug (fn _ => "    PAYLOAD: '" ^ String.to_hex (Byte.bytesToString payload) ^ " '" ^ Byte.bytesToString payload)

            fun assert_fin () = if fin then () else raise WebSocket ("opcode '" ^ opcode_to_string opcode ^ "' must be FINished.")

            fun ret packet_type =
              if fin
              then
                (packet_type, Word8Vector.concat (List.rev (payload :: payloads)))
              else
                p (SOME packet_type) (payload :: payloads)
          in
            case opcode of
                CONTINUATION =>
                (case packet_type of
                     NONE => raise WebSocket ("opcode CONTINUATION cannot initiate a sequence of packets.")
                   | SOME packet_type => ret packet_type)
              | TEXT => ret TXT
              | BINARY => ret BIN
              (* CONNECTION_CLOSE: discard any incomplete data we've buffered. *)
              (* FIXME parse payload? RFC 5.5.1 *)
              | CONNECTION_CLOSE => ( assert_fin ()
                                   ; connection_close (bin, bout) )
              | PING => ( assert_fin ()
                       ; ping bout payload
                       ; p packet_type payloads )
              | PONG => ( assert_fin ()
                       ; pong bout payload
                       ; p packet_type payloads )
          end
  in
    p NONE []
  end

fun mk_fns (bin, bout) : fns =
  { recv = receive_packets (bin, bout)
  , send = send_packet bout o Pair.mapFst packet_type_to_opcode
  }

fun handle_websocket mp (f : fns -> unit) : Http_server.handler =
  let
    fun handler_proc (bin, bout, rp as {req, ...}) =
        if Http_headers.lookup (#http_headers req) "Upgrade" <> SOME "websocket" (* FIXME case insensitive on "websocket" *)
           orelse Http_headers.lookup (#http_headers req) "Sec-WebSocket-Version" <> SOME "13"
           orelse not (Http_headers.defined (#http_headers req) "Sec-WebSocket-Key")
        then
          let
            open Html
            val uri = #uri (#request_line req)
            val html : Html.t =
                paragraph (str "The requested URL "
                               +++ href uri (str (Uri.to_string I uri))
                               +++ str " addresses a websocket.")
                          +++ paragraph (str "You submitted the following headers:")
                          +++ pre (Http_headers.to_string (#http_headers req))
          in
            Http_server.respond bout (Http_handlers.respond_with_html (Http_handlers.respond_bad_request html) rp)
          end
        else
          (* Respond with handshake. *)
          let
            val () = log_info (fn _ => "started")
            val sec_websocket_accept = websocket_accept_val (Option.valOf (Http_headers.lookup (#http_headers req) "Sec-WebSocket-Key")) (* FIXME two lookups on this field *)
            val http_headers = List.foldl (uncurry Http_headers.update) Http_headers.empty [
                  ("Upgrade", "websocket")
                , ("Connection", "Upgrade")
                , ("Sec-Websocket-Accept", sec_websocket_accept)
                ]
            val http_response : Response.t
                = { http_status = Http_codes.Switching_protocols
                  , http_headers = http_headers
                  , coding = SOME Transfer_coding.chunked (* FIXME plausibly NONE *)
                  }
            val () = Http_server.respond bout (http_response, CoSeq.empty ())
            val () = log_debug (fn _ => "handshake completed")
          in
            unwind (fn _ => ( log_debug (fn _ => "done, sending CONNECTION_CLOSE packet.")
                          ; connection_close (bin, bout)
                          ; log_debug (fn _ => "handle_websocket: DONE") ))
                   (mk_fns (bin, bout)) f
          end
  in
    (mp, Http_server.HandlerWithStream handler_proc)
  end

fun handle_echo (fns : fns) : unit =
  let
    fun go () =
      let
        val (pt, v) = #recv fns ()
      in
        print ("Http_websockets.echo: '" ^ packet_type_to_string pt ^ "' '" ^ Byte.bytesToString v ^ "'\n");
        case pt of
            EOF => ()
          | _ => ( #send fns (pt, CoSeq.return v); go () )
      end
  in
    go ()
  end

end
