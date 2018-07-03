(* FIXME HTTP structures. Biased towards a server presently.

*)

signature HTTP_DATE =
sig
  (* FIXME clean: RFC 1123 5.2.14 ?
   * Format dates in HTTP format.
   *
   * We cannot use the Date.fmt function in the basis library for this, because
   * it uses the current locale (and the current locale's names for days of
   * week and months), rather than the universal RFC 1123 date format.
   *)
  val format : Date.date -> string
end

structure Http_date :> HTTP_DATE =
struct

structure D = Date

val format_wd =
 fn D.Mon => "Mon" | D.Tue => "Tue" | D.Wed => "Wed"
    | D.Thu => "Thu" | D.Fri => "Fri" | D.Sat => "Sat"
    | D.Sun => "Sun"

val format_mon =
 fn D.Jan => "Jan" | D.Feb => "Feb" | D.Mar => "Mar"
    | D.Apr => "Apr" | D.May => "May" | D.Jun => "Jun"
    | D.Jul => "Jul" | D.Aug => "Aug" | D.Sep => "Sep"
    | D.Oct => "Oct" | D.Nov => "Nov" | D.Dec => "Dec"

fun format date =
    let
      fun lz2 num =
          let val str = Int.toString num
          in
            case size str of 1 => "0" ^ str | _ => str
          end
    in
      String.concat [
        format_wd (D.weekDay date), ", ", lz2 (D.day date), " ",
        format_mon (D.month date), " ", Int.toString (D.year date), " ",
        lz2 (D.hour date), ":", lz2 (D.minute date), ":",
        lz2 (D.second date), " GMT" ]
    end

end

(* RFC7230 section 3.2 -- Header Fields *)

signature HTTP_HEADERS =
sig
  include TABLE
    where type key = string

  type t = string table

  val content_length : string
  val content_type : string

  val to_string : t -> string (* FIXME RFC compliant *)
  val to_string_debug : t -> string
  val header_fieldsP : t ScanExt.parser (* FIXME generalise? *)
end

structure Http_headers :> HTTP_HEADERS =
struct

open Basics
open Library
open Scan
open ScanExt
open Symtab (* FIXME use an smlnj-lib structure instead? Also some interference with Scan *)

type t = string table

val content_length : string = "Content-Length"
val content_type : string = "Content-Type"

(* FIXME this is for HTTP. Also want one for debugging. *)
fun to_string (t : t) : string = (* FIXME inefficient, use String.concat? *)
    fold (fn (k, v) => fn str => str ^ k ^ ": " ^ v ^ crlf) t ""

fun to_string_debug (t : t) : string =
    fold (fn (k, v) => fn str => str ^ "'" ^ k ^ "': '" ^ v ^ "'\n") t ""

(* RFC7230 3.2.3 Whitespace *)

val is_whitespace : char -> bool = List.mem [#" ", #"\t"]
val wsP : ss parser = char_predP is_whitespace
val owsP : unit parser = many_unit wsP (* optional whitespace *)
val rwsP : unit parser = many1_unit wsP (* required whitespace *)
val bwsP : unit parser = owsP (* "bad" whitespace *)

(* RFC7230 3.2.6 Field Value Components *)

(* FIXME hugely inefficient *)
val tcharP = char_predP (List.mem [ #"!", #"#", #"$", #"%", #"&", #"'", #"*",
                                    #"+", #"-", #".", #"^", #"_", #"`", #"|", #"~" ])
                        || RFC2234.digitP || RFC2234.alphaP (* any VCHAR, except delimiters *)

(* RFC7230 3.2 Header Fields
   Erronenous. Adopt Simon Schueppel's proposal (errata id 4189).
   http://www.rfc-editor.org/errata_search.php?rfc=7230
 *)

val tokenP : ss parser = many1_ss tcharP

(* FIXME apparently field names are to be treated case-insensitively. Map to lowercase here? *)
val field_nameP : string parser = tokenP >> SS.string

val obs_foldP : char list parser = (* 3.2.4 yield one or more SP, or reject it *)
    (crlfP -- many1_unit wsP) >> K [#" "]

val field_owsP : char list parser =
    many_list (wsP >> ss_to_char) -- maybe_list obs_foldP >> op @ (* FIXME inefficient *)
val fwsP = field_owsP

val obs_textP = char_predP (fn c => let val x = Char.ord c in 0x80 <= x andalso x <= 0xFF end) (* opaque data *)
val field_vcharP : char parser = (* FIXME note 0x0-0x20, 0x7F are excluded here *)
    (RFC2234.vcharP || obs_textP) >> ss_to_char

val field_valueP : string parser =
    maybe_list (field_vcharP ::: (many_list ((field_owsP -- (field_vcharP >> Library.single)) >> op @) >> List.concat))
      >> String.implode (* FIXME inefficient *)

val header_fieldP : (string * string) parser =
    (field_nameP --| (charP #":" |--| fwsP)) -- (field_valueP --| fwsP)
                                             (* >> (fn (h, v) => ( print ("GOT HEADER: " ^ h ^ ": " ^ v ^ "\n"); (h, v))) *)

(* RFX 7230 3 Message Format FIXME "*(header-field CRLF)" *)
val header_fieldsP : t parser =
    many_list (header_fieldP--| crlfP)
      >> Symtab.make

end

(* FIXME RFC cite *)

signature METHOD =
sig
  datatype t
    = GET | HEAD | POST | PUT | DELETE | CONNECT | OPTIONS | TRACE
  val enum : t -> int
  val compare : t * t -> order
  val to_string : t -> string
  val methodP : t ScanExt.parser
end

structure Method :> METHOD =
struct

open Library
open Scan

datatype t
  = GET | HEAD | POST | PUT | DELETE | CONNECT | OPTIONS | TRACE

val enum : t -> int =
 fn GET => 0
  | HEAD => 1
  | POST => 2
  | PUT => 3
  | DELETE => 4
  | CONNECT => 5
  | OPTIONS => 6
  | TRACE => 7

fun compare (t0, t1) = Int.compare (enum t0, enum t1)

fun to_string m =
    case m of
        GET => "GET"
      | HEAD => "HEAD"
      | POST => "POST"
      | PUT => "PUT"
      | DELETE => "DELETE"
      | CONNECT => "CONNECT"
      | OPTIONS => "OPTIONS"
      | TRACE => "TRACE"

val methodP : t ScanExt.parser =
    let open ScanExt
    in
      (* MUST *)
      stringP "GET" >> K GET || stringP "HEAD" >> K HEAD
              (* OPTIONAL *)
              || stringP "POST" >> K POST
              || stringP "PUT" >> K PUT
              || stringP "DELETE" >> K DELETE
              || stringP "CONNECT" >> K CONNECT
              || stringP "OPTIONS" >> K OPTIONS
              || stringP "TRACE" >> K TRACE
    end

end

(* RFC7230 section 4 -- Transfer Codings *)

signature TRANSFER_CODING =
sig
  datatype t
    = chunked | compress | deflate | gzip | transfer_extension (* FIXME *)
  val to_string : t -> string
  val transfer_codingP : t ScanExt.parser
end

structure Transfer_coding :> TRANSFER_CODING =
struct

open Library
open Scan

datatype t
  = chunked | compress | deflate | gzip | transfer_extension (* FIXME *)

fun to_string tc =
    case tc of
        chunked => "chunked"
      | compress => "compress"
      | deflate => "deflate"
      | gzip => "gzip"
      | transfer_extension => raise FIXME "HTTP.Transfer_encoding: transfer_extension"

val transfer_codingP : t ScanExt.parser =
    let open ScanExt
    in
      stringP "chunked" >> K chunked (* 4.1 *)
   || stringP "compress" >> K compress || stringP "x-compress" >> K compress (* 4.2.1 *)
   || stringP "deflate" >> K deflate (* 4.2.2 *)
   || stringP "gzip" >> K gzip || stringP "x-gzip" >> K gzip (* 4.2.3 *)
    end

end

(* FIXME HTTP client requests *)
signature REQUEST =
sig
  type request_line =
       { method : Method.t
       , uri : Uri.curi
       , version : string (* FIXME abstract *)
       }

  type t =
       { request_line : request_line
       , http_headers : Http_headers.t
(*       , coding : Transfer_coding.t *)
       }

  val request_line_to_string : request_line -> string
  val to_string : t -> string
  val to_string_debug : t -> string

  val request_lineP : request_line ScanExt.parser
  val http_messageP : t ScanExt.parser

end

structure Request :> REQUEST =
struct

type request_line =
     { method : Method.t
     , uri : Uri.curi
     , version : string
     }

type t =
     { request_line : request_line
     , http_headers : Http_headers.t
(*     , coding : Transfer_coding.t *)
     }

fun request_line_to_string (rl : request_line) : string =
    Method.to_string (#method rl) ^ " " ^ Uri.to_string I (#uri rl) ^ " HTTP/" ^ (#version rl)

fun to_string (t : t) : string =
    request_line_to_string (#request_line t) ^ "\n" ^ Http_headers.to_string (#http_headers t)

fun to_string_debug (t : t) : string =
    request_line_to_string (#request_line t) ^ "\n" ^ Http_headers.to_string_debug (#http_headers t)

(* FIXME parsing *)

open Library
open Scan
open ScanExt

(*** RFC7230 section 2.6 -- Protocol Versioning ***)

val http_versionP : string parser =
    (stringP "HTTP" |-- charP #"/" |-- (RFC2234.digitP --| charP #".") -- RFC2234.digitP)
      >> (SS.string o SS.span)

(*** RFC7230 section 5.3 -- Request Target ***)

(* RFC7230 section 5.3.2 -- absolute-form. FIXME servers must accept these but clients should only use these for proxies. *)

val absolute_formP : Uri.curi parser =
    Uri.absoluteP

(* RFC7230 section 5.3.3 -- authority-form FIXME only for CONNECT requests *)

val authority_formP = Scan.fail

(* RFC7230 section 5.3.4 -- asterisk-form FIXME only for OPTIONS requests *)

val asterisk_formP = Scan.fail

val request_targetP : Uri.curi parser =
    Uri.origin_formP || absolute_formP || authority_formP || asterisk_formP

(* RFC7230 section 3.1.1 -- Request Line points to RFC7231 4.1 -- Request Methods / Overview FIXME *)

val request_lineP : request_line parser =
    (Method.methodP --| RFC2234.spaceP) -- (request_targetP --| RFC2234.spaceP) -- http_versionP --| crlfP
      >> (fn ((m, u), v) => { method = m, uri = u, version = v })

(* FIXME parser top-level *)
open ScanExt

(* RFC2730 section 3.1 Start Line *)

val start_lineP : request_line parser =
    request_lineP (* || status_lineP FIXME we're a server *)

(* RFC7230 section 3 -- Message Format. *)

val http_messageP : t parser =
    (start_lineP -- Http_headers.header_fieldsP) --| crlfP
      >> (fn (rl, hs) => { request_line = rl, http_headers = hs })

end

(* FIXME HTTP server responses, created by the user. Looks like a CGI response. *)
signature RESPONSE =
sig
  type t =
       { (* FIXME wired into the server: version : string (* Code.version *) *)
         http_status : Http_codes.status
       , http_headers : Http_headers.t
       , coding : Transfer_coding.t option (* FIXME NONE means let the server figure it out *)
       (* FIXME , flush : bool *)
       }
  val to_string : t -> string
  val write_to_outstream : BinIO.outstream -> t -> unit
end

structure Response :> RESPONSE =
struct

type t =
     { (* version : string (* Code.version *) *)
       http_status : Http_codes.status
     , http_headers : Http_headers.t
     , coding : Transfer_coding.t option
     (* , flush : bool *)
     }

fun to_string (t : t) : string =
    raise FIXME "Response.to_string"

(* FIXME clunky, inefficient. Fold the extra headers in with the user's. *)
fun write_to_outstream (bout : BinIO.outstream) (t : t) : unit =
    let val response =
            String.concat [
              "HTTP/1.1 ", Http_codes.string_of_status (#http_status t), crlf
              , "Date: ", Http_date.format (Date.fromTimeLocal (Time.+ (Time.now (), Date.localOffset ()))), crlf
              , "Server: jarvis", crlf
              ] ^ Http_headers.to_string (#http_headers t) ^ crlf
    in
      Log.log {name="Response.write_to_outstream", level=Log.Debug}
              (fn () => response);
      BinIO.output (bout, Byte.stringToBytes response)
    end

end

  (* FIXME need sharing constraints to get this to go
signature HTTP =
sig
  structure Headers : HEADERS
  structure Method : METHOD
  structure Transfer_coding : TRANSFER_CODING
  structure Request : REQUEST  where Method.t = Method.t
  structure Response : RESPONSE
end

structure Http :> HTTP =
struct

structure Headers = Headers
structure Method = Method
structure Transfer_coding = Transfer_coding
structure Request = Request
structure Response = Response

end
*)
