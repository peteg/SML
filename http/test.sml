(* FIXME MOVE testing detritus *)

fun html_handler mp : Http_server.handler =
  let
    open Html
    val http_headers = Http_headers.empty
    val html_headers = []
    val html = h1 (str "HELLO WORLD!")
  in
    Http_handlers.handle_html mp
      (K { http_status = Http_codes.OK
         , http_headers = http_headers
         , html_headers = html_headers
         , html = html })
  end

val () = Http_server.add_handler (html_handler {method=Method.GET, path=["test"]})
val () = Http_server.add_handler (Http_websockets.handle_websocket {method=Method.GET, path=["echo"]}
                                                                   Http_websockets.handle_echo)

(* Actual services. FIXME The path is dodgy. *)
(* FIXME install /favicon.ico handler? *)
(* FIXME reduce verbosity here *)
val () = Http_server.add_handler (Http_handlers.handle_html {method=Method.GET, path=[]}
                                                            Http_handlers.respond_not_found)

val () =
    let
      val cwd = OS.FileSys.getDir () (* FIXME should split into path elts *)
    in
      Http_server.add_handler (Http_handlers.handle_filesystem {method=Method.GET, path=["dir"]}
                                                               {doc_root = [cwd]})
    end

val () = Generic_server_threaded.serve 8080 Http_server.handle_http_request
