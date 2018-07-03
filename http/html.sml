(* Based on Andy Gill/Björn Bringert's Haskell (X)HTML lib.

   FIXME can get a bit more safety with more types in the interface.
   FIXME want HTML5 and possibly XML too.

https://mathiasbynens.be/notes/xhtml5
*)

(* FIXME just guessing *)
infixr 1 +++
infix  2 @! @!!

signature HTML =
sig

  (** FIXME type of HTML elements *)
  type t
  (** FIXME type of HTML attributes *)
  type attr = string * string
  type attrs = attr list

  (** Type of HEAD elements. *)
  datatype h
    = ContentType of string
    | CSS of string
    | Script of string
    | Style of string
    | Title of string

  val standard_headers : h list

  (** FIXME combinators *)
  val noHtml : t
  val append : t * t -> t
  val concat : t list -> t
  val applyAttrs : t * attrs -> t
  val applyAttrsFn : (t -> t) * attrs -> t -> t

  val +++ : t * t -> t (* = append *)
  val @! : t * attrs -> t (* = applyAttrs *)
  val @!! : (t -> t) * attrs -> t -> t (* = applyAttrsFn *)

  val str : string -> t

  val canvas : t -> t
  val div_ : t -> t
  val h1 : t -> t
  val h2 : t -> t
  val hr : t
  val href : Uri.curi -> t -> t
  val paragraph : t -> t
  val pre : string -> t
  val span : t -> t
  val svg : t (* FIXME allow content *)
  val ul : t CoSeq.t -> t

  (** JavaScript *)
  val js : string -> t

  (** Widgets *)
  val button : t -> t

  (** Attributes *)
  val height : int -> attr
  val id : string -> attr
  val src : string -> attr
  val width : int -> attr

  (** FIXME render *)
  val render_fragment : t -> Buffer.t CoSeq.t
  val render : h list * t -> Buffer.t CoSeq.t

  val null : t -> bool
end

structure Html :> HTML =
struct

type tag = string

type attr = string * string
type attrs = attr list

datatype htmlElement =
    HtmlLiteral of Buffer.t (* will not be escaped *)
  | HtmlString of string (* will be escaped *)
  | HtmlTag of tag * attrs * html
withtype html = htmlElement CoSeq.t

type t = html

val noHtml : html = CoSeq.empty ()
val append : html * html -> html = CoSeq.append
val concat : html list -> html = CoSeq.concatl

fun tag (tag : tag) (html : html) : html =
  CoSeq.return (HtmlTag (tag, [], html))

fun itag (itag : tag) : html =
  tag itag noHtml

fun applyAttrs (html : html, attrs' : attrs) : html =
  CoSeq.map
    (fn t => case t of
               HtmlTag (tag, attrs, html) => HtmlTag (tag, attrs' @ attrs, html)
             | elt => elt)
    html

fun applyAttrsFn (f : html -> html, attrs : attrs) (html : html) : html =
  applyAttrs (f html, attrs)

(* Operators *)
val op +++ : t * t -> t = append
val op @! = applyAttrs
val op @!! = applyAttrsFn

(* HTML constructors. *)
val buf : Buffer.t -> html = CoSeq.return o HtmlLiteral
val lit : string -> html = buf o Buffer.string
val str : string -> html = CoSeq.return o HtmlString

val anchor = tag "a"

val canvas = tag "canvas"
val div_ = tag "div"
val h1 = tag "h1"
val h2 = tag "h2"
val hr = itag "hr"
val li = tag "li"
val paragraph = tag "p"
val pre = tag "pre" o lit
val span = tag "span"
val svg = itag "svg"

val ul : html CoSeq.t -> html =
  tag "ul" o CoSeq.join o CoSeq.map li

fun href (curi: Uri.curi) (t: t) : t =
  anchor t @! [("href", Uri.to_string I curi)] (* FIXME leak user auth info ?? *)

val js = tag "script" o lit

val button = tag "button"

fun height h = ("height", Int.to_string h)
fun id str = ("id", str)
fun src str = ("src", str)
fun width w = ("width", Int.to_string w)

(* Top-level stuff *)

(* Elements of <head> *)

datatype h =
    ContentType of string
  | CSS of string
  | Script of string
  | Style of string
  | Title of string

fun render_header (h : h) : html =
  case h of
    ContentType s => lit s
  | CSS s => lit s
  | Script s => itag "script" @! [("src", s)]
  | Style s => tag "style" (str s)
  | Title s => tag "title" (str s)

(* FIXME *)
val standard_headers =
  [ ContentType "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"
  ]

(* FIXME HTML 5 strict? what are the headers? *)

fun xhtml (headers : h list) (body : t) : t =
    lit "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
+++ (tag "html" @!! [("xmlns", "http://www.w3.org/1999/xhtml"), ("xml:lang", "en"), ("lang", "en")])
      (tag "head" (CoSeq.join (CoSeq.from_list (List.map render_header headers)))
   +++ tag "body" body)

fun html5 (headers : h list) (body : t) : t =
    lit "<!DOCTYPE html>"
+++ tag "html"
      (tag "head" (CoSeq.join (CoSeq.from_list (List.map render_header headers)))
   +++ tag "body" body)

(*
   FIXME
   Working hypothesis: strings could be huge. escapes are rare.
   Ergo want to do as little copying as possible.
*)

val render_string : string -> Buffer.t =
  let
    fun isEscapeChar c = Char.ord c < 0x80 orelse List.mem [#"<", #">", #"&", #"\""] c
    fun escapeChar c : Buffer.t =
        case c of
            #"<" => Buffer.string "&lt;"
          | #">" => Buffer.string "&gt;"
          | #"&" => Buffer.string "&amp;"
          | #"\"" => Buffer.string "&quot;"
          | c => if Char.ord c < 0x80
                then Buffer.char c
                else Buffer.string ("&#" ^ Int.toString (Char.ord c) ^ ";") (* FIXME correct for unicode? *)
    fun escapeSubString (b : Buffer.t) (ss : Substring.substring) : Buffer.t =
        let val (l, r) = Substring.splitl isEscapeChar ss
            val b = Buffer.append b (Buffer.substring l)
        in
          if Substring.isEmpty r
          then b
          else
            let
              val b = Buffer.append b (escapeChar (Substring.sub (r, 0)))
            in
              escapeSubString b (Substring.slice (r, 1, NONE))
            end
        end
  in
    escapeSubString Buffer.empty o Substring.full
  end

(* FIXME escaping in attrs? *)
(* FIXME assume tags are small. Some attributes may be large. *)
fun render_tag (empty: bool) (tag: tag) (attrs: attrs) : Buffer.t =
    let fun showAttr (a, v) = " " ^ a ^ "=\"" ^ v ^ "\""
    in
      Buffer.string ("<" ^ tag ^ String.concat (List.map showAttr attrs)
                     ^ (if empty then "/>" else ">"))
    end

fun render_end_tag (tag : tag) : Buffer.t =
    Buffer.string ("</" ^ tag ^ ">")

fun render_htmlElement (elt : htmlElement) : Buffer.t CoSeq.t =
  case elt of
      HtmlLiteral b => CoSeq.return b
    | HtmlString str => CoSeq.return (render_string str)
    | HtmlTag (tag, attrs, html) =>
      CoSeq.append ( CoSeq.from_list [render_tag false tag attrs]
                   , CoSeq.append ( render_Html html
                                  , CoSeq.from_list [render_end_tag tag]) )
and render_Html (html: html) : Buffer.t CoSeq.t =
  CoSeq.concat (CoSeq.map render_htmlElement html)

val render_fragment : t -> Buffer.t CoSeq.t =
  render_Html

fun render (headers: h list, body: t) : Buffer.t CoSeq.t =
  render_Html (html5 headers body)

val null = CoSeq.null

end
