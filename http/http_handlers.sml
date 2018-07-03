(* Generic HTTP handlers ala Apache. *)

(*

FIXME Adopt conventions:

handle_ -- hand it straight to the server
respond_ -- needs some composition first

*)

signature HTTP_HANDLERS =
sig

  val text_utf8_http_headers : Http_headers.t
  val html_utf8_http_headers : Http_headers.t

  (* FIXME this needs more thought. Sometimes we need
     Http_server.request but often enough we don't. *)

  type html_response = { http_status: Http_codes.status
                       , http_headers: Http_headers.t
                       , html_headers : Html.h list
                       , html: Html.t }

  type html_request_response = Http_server.request -> html_response

  val respond_with_html : html_request_response -> Http_server.request_response

  val respond_with_status : Http_codes.status * Html.t -> html_request_response

  val respond_bad_request : Html.t -> html_request_response
  val respond_forbidden : Html.t -> html_request_response
  val respond_not_found : html_request_response

  (* FIXME permuting argument order here may help. *)
  (* FIXME better name than "path" *)
  val handle_html : {method: Method.t, path: Uri.path} -> html_request_response -> Http_server.handler
  val handle_filesystem : {method: Method.t, path: Uri.path} -> {doc_root: Uri.path} -> Http_server.handler

end

structure Http_handlers :> HTTP_HANDLERS =
struct

open Basics

type html_response = { http_status: Http_codes.status
                     , http_headers: Http_headers.t
                     , html_headers : Html.h list
                     , html: Html.t }
type html_request_response = Http_server.request -> html_response

(* FIXME useful? *)
val text_utf8_http_headers : Http_headers.t =
    List.foldl (uncurry Http_headers.update) Http_headers.empty [
      (Http_headers.content_type, Mime.text_plain ^ "; charset=utf-8")
    ]

val html_utf8_http_headers : Http_headers.t =
    List.foldl (uncurry Http_headers.update) Http_headers.empty [
      (Http_headers.content_type, Mime.text_html ^ "; charset=utf-8")
    ]

fun respond_with_html (f : html_request_response) : Http_server.request_response =
  fn (rp (* as (req, path) *)) =>
     let
       val {http_status, http_headers, html_headers, html} = f rp
       val http_response : Response.t
           = { http_status = http_status
             , http_headers = http_headers
             , coding = SOME Transfer_coding.chunked (* FIXME plausibly NONE *)
             }
       val out : string CoSeq.t = CoSeq.chunk 16384 (Html.render (html_headers, html)) (* FIXME magic *)
     in
       (http_response, out)
     end

fun handle_html method_path (hrr : html_request_response) : Http_server.handler =
  (method_path, Http_server.Handler (respond_with_html hrr))

fun respond_with_status (http_status, info) : html_request_response =
  fn _ (* (req, path) *) =>
     let
       open Html
       val http_headers = html_utf8_http_headers
       val title = Http_codes.string_of_status http_status
       val html_headers = [Title title]
       val html =
           h1 (str title)
              +++ hr
              +++ info
     in
       { http_status = http_status
       , http_headers = http_headers
       , html_headers = html_headers
       , html=html }
     end

fun respond_bad_request (info : Html.t) : html_request_response =
  respond_with_status (Http_codes.Bad_request, info)

fun respond_forbidden (info : Html.t) : html_request_response =
  respond_with_status (Http_codes.Forbidden, info)

val respond_not_found : html_request_response =
 fn {req, ...} =>
    let
      val () = Log.log {name="Http_handlers.respond_not_found", level=Log.App}
                       (fn () => Request.request_line_to_string (#request_line req)) (* FIXME Request.to_string req) *)
      open Html
      val http_status = Http_codes.Not_found
      val http_headers = html_utf8_http_headers
      val title = Http_codes.string_of_status http_status
      val html_headers = [Title title]
      val html =
          h1 (str title)
             +++ hr
             +++ paragraph (str "The requested URL /" (* FIXME path_to_string is relative? *)
                                +++ str (Uri.to_string (K "") (#uri (#request_line req)))
                                +++ str " was not found on this server.")
    in
      { http_status = http_status
      , http_headers = http_headers
      , html_headers = html_headers
      , html=html }
    end

(* File system handlers. *)

structure FS = Posix.FileSys
structure IO = Posix.IO
structure Path = OS.Path

(* FIXME needs a lot of care and work. May not be worth it. *)
(* FIXME racy but we'd need transactions on the filesystem to do it right. *)
(* FIXME sizes, etc. *)
(* FIXME links need to be relative paths, can't point outside of ... ? *)
(* FIXME CSS classes based on MIME type *)
(* FIXME add parent link? *)
fun respond_directory {doc_root : Uri.path, fs_path : Uri.path, uri_root : Uri.path} : html_request_response =
  fn {path: Uri.path, ...} =>
     let
       open Html
       val log_info = Log.log {name = "Http_handlers.respond_directory", level=Log.Info}
       val fs_path_str = Uri.path_to_string fs_path
       val doc_root_str = Uri.path_to_string doc_root
       val uri_path_str = Uri.path_to_string (uri_root @ path) (* FIXME this should be the original request path *)
       val () = log_info (fn () => "processing '" ^ fs_path_str ^ "'")
       val seq = FileSys.find {depth = 1, follow = false, roots = [fs_path_str]}
       val () = Log.guard Log.Info (CoSeq.app (fn (path, _) => log_info (K path))) seq

       fun render_file (canonical_file_path, st) =
         let
           val rel_file_path = OS.Path.mkRelative {path=canonical_file_path, relativeTo=doc_root_str}
           val file_uri_path = uri_root @ [rel_file_path]
           val link = case ScanExt.run_parser Uri.uriP (Uri.path_to_string file_uri_path) of
                          NONE => raise FIXME ("respond_directory: uri parse failed: '" ^ Uri.path_to_string file_uri_path ^ "'")
                        | SOME uri => uri
         in
           if OS.Path.mkRelative {path=canonical_file_path, relativeTo=fs_path_str} = Path.currentArc
           then
             Html.noHtml
           else if FS.ST.isReg st
           then
             href link (str (Path.file rel_file_path)) @! [("class", "file " ^ Mime.type_of rel_file_path)]
           else if FS.ST.isDir st
           then
             href link (str (Path.file rel_file_path)) @! [("class", "directory")]
           else
             Html.str (Path.file rel_file_path) @! [("class", "filetype_unknown")]
         end

       fun render_ls list =
         let
           (* FIXME: uri_path_str should always have a leading / ? *)
           val title = "Directory listing for " ^ uri_path_str
           val html =
               h1 (str "Directory listing for " +++ span (str uri_path_str)) (* FIXME CSS class for path contents info in *)
                  +++ ul list
                  +++ hr(*
                           +++ str info FIXME *)
         in
           { http_status = Http_codes.OK
           , http_headers = html_utf8_http_headers (* FIXME utf8 arguable: depends on the filename encoding *)
           , html_headers = [Html.Title title]
           , html = html }
         end
     in
       seq
         |> CoSeq.map render_file
         |> CoSeq.filter (not o Html.null)
         |> render_ls
     end

(* FIXME exceptions *)
(* FIXME close file handle, or maybe BinIO does that for us? *)
fun handle_file {fd : FS.file_desc} : Http_server.request_response =
  fn {path, ...} =>
     let
       val mime_type = Mime.type_of (Uri.path_to_string path) (* FIXME purely filename, not contents. Don't know what text encoding. *)
       val http_headers = List.foldl (uncurry Http_headers.update) Http_headers.empty [
             (Http_headers.content_type, mime_type)
           ]
       val response : Response.t
           = { http_status = Http_codes.OK
             , http_headers = http_headers
             , coding = SOME Transfer_coding.chunked (* FIXME not for us to say? *)
             }
       val rd = IO.mkBinReader {fd=fd, initBlkMode=false, name=Uri.path_to_string path}
       val stream = BinIO.mkInstream
                      (BinIO.StreamIO.mkInstream (rd, Word8Vector.fromList []))
       val out : string CoSeq.t =
           CoSeq.return (Byte.bytesToString (BinIO.inputAll stream)) (* FIXME *)
       val () = BinIO.closeIn stream
     in
       (response, out)
     end

(* FIXME 'path' comes from the request. It needs careful vetting:
    - should not contain ".."
    - probably gets normalised as part of the Uri parsing?
*)
(* FIXME exceptions. Tricky: close file handle if open. Otherwise leave it to the handlers to clean up? *)
fun handle_file_or_directory {doc_root, fs_path, uri_root} : Http_server.request_response =
  fn rp =>
     let
       val fname = Uri.path_to_string fs_path
       val fd = FS.openf (fname, FS.O_RDONLY, (FS.O.flags []))
       val st = FS.fstat fd
     in
       if FS.ST.isReg st
       then
         handle_file {fd=fd} rp
       else
         ( IO.close fd;
           if FS.ST.isDir st
           then
             respond_with_html (respond_directory {doc_root=doc_root, fs_path=fs_path, uri_root=uri_root}) rp
           else
             respond_with_html respond_not_found rp (* FIXME any other cases? *)
         )
     end
     handle (e as OS.SysErr (_, SOME syserr)) =>
            if syserr = Posix.Error.noent
            then
              respond_with_html respond_not_found rp
            else
              Platform.reraise e

(* FIXME doc_root should be an absolute path here, in case something monkeys with the process's CWD *)
(* FIXME no reason to belive uri_root has anything to do with path. Suggests a change toe the handler datatype *)
(* FIXME allow extra HTML/HTTP headers *)
fun handle_filesystem (method_path as {path=uri_root, ...}) {doc_root} : Http_server.handler =
  let
    val name = "Http_handlers.handle_filesystem"
    val () = Log.log {name=name, level=Log.App}
                     (fn () => "Serving doc_root '" ^ Uri.path_to_string doc_root ^ "' at URI path '" ^ Uri.path_to_string uri_root ^ "'.")
    fun handler (rp as {req, path}) =
        let
          val fs_path = doc_root @ path
          val () = Log.log {name=name, level=Log.Info}
                           (fn () => Request.request_line_to_string (#request_line req) ^ " -> " ^ Uri.path_to_string fs_path)
        in
          handle_file_or_directory {doc_root=doc_root, fs_path=fs_path, uri_root=uri_root} rp
        end
  in
    (method_path, Http_server.Handler handler)
  end

end
