(* MIME types

FIXME Parse mime.types from Apache. Map extensions to MIME types. Parse them too.

*)

signature MIME =
sig
  val application_javascript : string
  val application_json : string
  val application_octet_stream : string
  val application_pdf : string
  val application_xhtml_xml : string

  val image_jpeg : string

  val text_css : string
  val text_html : string
  val text_plain : string

  (** Guess the MIME type for the given extension. *)
  val type_of : string -> string
end

structure Mime : MIME =
struct

val application_javascript = "application/javascript"
val application_json = "application/json"
val application_octet_stream = "application/octet-stream"
val application_pdf = "application/pdf"
val application_xhtml_xml = "application/xhtml+xml"

val image_gif = "image/gif"
val image_jpeg = "image/jpeg"
val image_png = "image/png"
val image_svg = "image/svg+xml" (* FIXME verify *)


val text_css = "text/css"
val text_html = "text/html"
val text_plain = "text/plain"

(* FIXME non-canonical *)
val text_x_sml_source = "text/x-sml-source"

(* FIXME could normalize in various ways: lower case, ... *)
(* FIXME should just slirp apache's mime.types *)
fun type_of filename =
 case OS.Path.ext filename of
     NONE => application_octet_stream
   | SOME ext =>
     case ext of
         "css" => text_css
       | "html" => text_html
       | "js" => application_javascript
       | "json" => application_json
       | "pdf" => application_pdf
       | "sml" => text_x_sml_source
       | "txt" => text_plain

       | "gif" => image_gif
       | "jpeg" => image_jpeg
       | "jpg" => image_jpeg
       | "png" => image_png
       | "svg" => image_svg

       | _ => application_octet_stream

end
