(*

Based on Haskell's Network.URI, originally by Graham Klyne.
http://hackage.haskell.org/package/network-uri
Chapter-and-verse citing of the RFC is due to him and other Haskellites.
Licence as follows:

--------------------------------------------------------------------------------
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  Distributed as free software under the following license.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  - Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--  notice, this list of conditions and the following disclaimer in the
--  documentation and/or other materials provided with the distribution.
--
--  - Neither name of the copyright holders nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDERS OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
--  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
--  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
--  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------

FIXME have a constructor that takes paths into URIs efficiently. Probably need to escape.
FIXME Probably different enough now/soon that the licence doesn't apply.
FIXME no support for utf8 here. I mean, we'll unescape to utf8, but SML is oblivious to that.
FIXME get serious about string representations.
FIXME probably don't want to convert stuff here; provide higher-level
functions to do so.

FIXME does this make sense: [] means no path, [""] means /

FIXME testing:
http://skew.org/uri/uri_tests.html
http://lists.w3.org/Archives/Public/uri/2004Feb/0108.html
http://lists.w3.org/Archives/Public/www-qa/2001Aug/0000.html
http://www.w3.org/wiki/UriTesting
https://github.com/Sporkmonger/Addressable/blob/master/spec/addressable/uri_spec.rb
Scrape other language's testsuites.

*)

signature URI =
sig
  (* The authority value within a URI *)
  type auth =
    { userInfo   : string           (* anonymous@ *)
    , regName    : string           (* www.haskell.org *)
    , port       : int option       (* :42 *)
    }

  (* FIXME describe: split on '/' ? Certified paths? (elements should not contain the path separator) *)
  type path = string list

  type uri =
    { scheme     : string           (* foo: *)
    , authority  : auth option      (* //anonymous@www.haskell.org:42 *)
    , path       : path             (* /ghc *)
    , query      : string           (* ?query FIXME want to decode these *)
    , fragment   : string           (* #frag *)
    }

  (* Certified types *)
  type cauth
  type curi

  (* Constructors *)
  val null : curi (* Blank URI FIXME kosher? *)
  val mk_cauth : auth -> cauth
  val mk_curi : uri -> curi

  (* Predicates *)
  val is_absolute : curi -> bool
  val is_relative : curi -> bool

  (* Parsing *)
  val uriP : curi ScanExt.parser
  val referenceP : curi ScanExt.parser
  val relative_referenceP : curi ScanExt.parser
  val absoluteP : curi ScanExt.parser

  val origin_formP : curi ScanExt.parser

  (* Projections *)
  val cauth_to_auth : cauth -> auth
  val curi_to_uri : curi -> uri

  (* Printing *)
  val auth_to_string : (string -> string) -> cauth -> string (* fn: what to do with user info *)
  val path_to_string : string list -> string
  val to_string : (string -> string) -> curi -> string (* fn: what to do with user info *)

end

structure Uri :> URI =
struct

open Basics
open Library
open Scan
open ScanExt

type auth =
     { userInfo   : string
     , regName    : string
     , port       : int option
     }

type path = string list

type uri =
     { scheme     : string
     , authority  : auth option
     , path       : path
     , query      : string
     , fragment   : string
     }

type cauth = auth
type curi = uri

val cauth_to_auth = Fn.id
val curi_to_uri = Fn.id

val null : uri =
    { scheme     = ""
    , authority  = NONE
    , path       = []
    , query      = ""
    , fragment   = ""
    }

fun mk_cauth _ = raise FIXME "mk_cauth"
fun mk_curi _ = raise FIXME "mk_curi"

fun auth_to_string userinfomap {userInfo, regName, port} =
    "//" ^ (if userInfo = "" then "" else userinfomap userInfo ^ "@")
         ^ regName
         ^ (case port of NONE => "" | SOME p => ":" ^ Int.toString p)

(* FIXME guesswork: [] = "", i.e. no leading slash, paths are relative. *)
fun path_to_string path =
  String.concatWith "/" path

fun to_string userinfomap
              ({ scheme
               , authority
               , path
               , query
               , fragment
               } : curi) =
        scheme
      ^ Option.option (fn _ => "", auth_to_string userinfomap) authority
      ^ "/" ^ path_to_string path
      ^ (if query = "" then "" else "?" ^ query)
      ^ (if fragment = "" then "" else "#" ^ fragment)

fun is_absolute ({scheme, ...}: uri) = (scheme <> "")
val is_relative = not o is_absolute

(* Escaping *)

fun escape_char (p : char -> bool) (c : char) : string =
  if p c then
    "0x" ^ Int.fmt StringCvt.HEX (Char.ord c) (* FIXME pad *)
  else
    str c

(* URI parsing, based on Larry Paulson's / Isabelle's venerable scanning combinators. *)

val is_scheme_char: char -> bool =
  is_ascii_alpha_digit orf List.mem [#"+", #"-", #"."]

val is_gen_delim : char -> bool =
  List.mem [#":", #"/", #"?", #"#", #"[", #"]", #"@"]

val is_sub_delim : char -> bool =
  List.mem [#"!", #"$", #"&", #"'", #"(", #")", #"*", #"+", #",", #";", #"="]

(* RFC3986, section 2.1
   Parse and return a 'pct-encoded' sequence
*)

val escapedP : char parser =
  let fun unescape_char (s : string) : char =
          case StringCvt.scanString (Int.scan StringCvt.HEX) s of
              NONE => Scan.fail ()
            | SOME i => Char.chr i
  in
    (charP #"%" |-- ascii_hexP -- ascii_hexP) >> (unescape_char o SS.string o SS.span)
  end

(* RFC3986, section 2.2 *)
fun is_reserved (c : char) : bool =
  is_gen_delim c orelse is_sub_delim c

(* RFC3986, section 2.3
--
-- |Returns 'True' if the character is an \"unreserved\" character in
--  a URI.  These characters do not need to be escaped in a URI.  The
--  only characters allowed in a URI are either \"reserved\",
--  \"unreserved\", or an escape sequence (@%@ followed by two hex digits).
--
*)

val is_unreserved : char -> bool =
  is_ascii_alpha_digit orf List.mem [#"-", #"_", #".", #"~"]

val unreserved_charP : char parser =
  char_predP is_unreserved >> ss_to_char

(* helper function for pchar and friends *)
fun ucharP (extras : char -> bool) : char parser =
  unreserved_charP
    || escapedP
    || char_predP (is_sub_delim orf extras) >> ss_to_char

(* RFC3986, section 3.2.1 *)

val userinfoP : string parser =
  many_list (ucharP (List.mem [#";", #":", #"&", #"=", #"+", #"$", #","]))
    --| charP #"@" >> String.implode

(* RFC3986, section 3.2.2 *)

(*
ipLiteral :: URIParser String
ipLiteral =
    do  { char '['
        ; ua <- ( ipv6address <|> ipvFuture )
        ; char ']'
        ; return $ "[" ++ ua ++ "]"
        }
    <?> "IP address literal"

ipvFuture :: URIParser String
ipvFuture =
    do  { char 'v'
        ; h <- hexDigitChar
        ; char '.'
        ; a <- many1 (satisfy isIpvFutureChar)
        ; return $ 'v':h:'.':a
        }

isIpvFutureChar :: Char -> Bool
isIpvFutureChar c = isUnreserved c || isSubDelims c || (c==';')

ipv6address :: URIParser String
ipv6address =
        try ( do
                { a2 <- count 6 h4c
                ; a3 <- ls32
                ; return $ concat a2 ++ a3
                } )
    <|> try ( do
                { string "::"
                ; a2 <- count 5 h4c
                ; a3 <- ls32
                ; return $ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 0
                ; string "::"
                ; a2 <- count 4 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 1
                ; string "::"
                ; a2 <- count 3 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 2
                ; string "::"
                ; a2 <- count 2 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 3
                ; string "::"
                ; a2 <- h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 4
                ; string "::"
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 5
                ; string "::"
                ; a3 <- h4
                ; return $ a1 ++ "::" ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 6
                ; string "::"
                ; return $ a1 ++ "::"
                } )
    <?> "IPv6 address"

opt_n_h4c_h4 :: Int -> URIParser String
opt_n_h4c_h4 n = option "" $
    do  { a1 <- countMinMax 0 n h4c
        ; a2 <- h4
        ; return $ concat a1 ++ a2
        }

ls32 :: URIParser String
ls32 =  try ( do
                { a1 <- h4c
                ; a2 <- h4
                ; return (a1++a2)
                } )
    <|> ipv4address

h4c :: URIParser String
h4c = try $
    do  { a1 <- h4
        ; char ':'
        ; notFollowedBy (char ':')
        ; return $ a1 ++ ":"
        }

h4 :: URIParser String
h4 = countMinMax 1 4 hexDigitChar
*)

val dec_octet =
  between_ss 1 3 ascii_digitP
    >> (fn ss =>
           case Int.fromString (SS.string ss) of
               NONE => Scan.fail ()
             | SOME i => if i > 255 then Scan.fail () else ss)

val ipv4addressP : string parser =
  between_ss 4 4 (dec_octet --| charP #".") >> SS.string

val reg_nameP : string parser =
  between_list 0 255 (unreserved_charP || escapedP || char_predP is_sub_delim >> ss_to_char)
    >> String.implode
(* FIXME    <?> "Registered name" *)

(* FIXME refer back to uri.hs for details here *)
val hostP : string parser =
  ((* FIXME ipv6 ip_literalP ||*) (* FIXME parsec try *) ipv4addressP || reg_nameP) >> (fn x => (print ("hostP: " ^ x ^ "\n"); x))

(* RFC3986, section 3.2.3 *)
val portP : ss parser =
  charP #":" |-- ascii_digitsP (* FIXME want many1 sort of behaviour I think *)

(*
--
--  RFC3986, section 3.3
--
--   path          = path-abempty    ; begins with "/" or is empty
--                 / path-abs        ; begins with "/" but not "//"
--                 / path-noscheme   ; begins with a non-colon segment
--                 / path-rootless   ; begins with a segment
--                 / path-empty      ; zero characters
--
--   path-abempty  = *( "/" segment )
--   path-abs      = "/" [ segment-nz *( "/" segment ) ]
--   path-noscheme = segment-nzc *( "/" segment )
--   path-rootless = segment-nz *( "/" segment )
--   path-empty    = 0<pchar>
--
--   segment       = *pchar
--   segment-nz    = 1*pchar
--   segment-nzc   = 1*( unreserved / pct-encoded / sub-delims / "@" )
--
--   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
*)

val pcharP : char parser = ucharP (fn c => c = #":" orelse c = #"@")

val segmentNzP : string parser = many1_list pcharP >> String.implode
val segmentNzcP : string parser = many1_list (ucharP (fn c => c = #"@")) >> String.implode

val segmentP : string parser = many_list pcharP >> String.implode
val slash_segmentP : string parser =
  charP #"/" |-- segmentP

val path_rootlessP : string list parser =
  segmentNzP ::: many_list slash_segmentP

val path_no_schemeP : string list parser =
  segmentNzcP ::: many_list slash_segmentP

val path_absP : path parser =
  charP #"/" |-- maybe_list path_rootlessP (* FIXME probably need to preserve the root somehow *)

val path_abemptyP : string list parser = many_list slash_segmentP

(* RFC3986, section 3.4 FIXME want to further decode these, create an assoc list. *)

val ucharP' : char parser = ucharP (List.mem [#":", #"@", #"/", #"?"])

val queryP : string parser = maybe_list (charP #"?" |-- many_list ucharP') >> String.implode

(* RFC3986, section 3.5 *)

val fragmentP : string parser = maybe_list (charP #"#" |-- many_list ucharP') >> String.implode

(* RFC3986, section 3.1 *)

val schemeP : string parser =
  ascii_alpha_charP -- many_ss (char_predP is_scheme_char) --| charP #":" >> (SS.string o viewSS "scheme" o SS.span)

(* RFC3986, section 3.2 *)
val authorityP : cauth option parser =
  maybe ""  userinfoP -- hostP -- maybe NONE (portP >> (SOME o SS.string))
    >> (fn ((uu, uh), up) => SOME { userInfo = uu
                                , regName = uh
                                , port = Option.mapPartial Int.fromString up
                                })

(*
--  RFC3986, section 3
--
--   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
--
--   hier-part   = "//" authority path-abempty
--               / path-abs
--               / path-rootless
--               / path-empty
*)

val hier_partP : (cauth option * string list) parser =
  (stringP "//" |-- Scan.catch (authorityP -- path_abemptyP)) (* FIXME committed choice *)
    || (path_absP >> (fn up => (NONE, up)))
    || (path_rootlessP >> (fn up => (NONE, up)))
    || Scan.succeed (NONE, [])

val uriP : curi parser =
  (schemeP || Scan.succeed "") -- hier_partP -- queryP -- fragmentP
    >> (fn (((us, (ua, up)), uq), uf) => { scheme = us
                                       , authority = ua
                                       , path = up
                                       , query = uq
                                       , fragment = uf
                                       })

(*
--  Reference, Relative and Absolute URI forms

--  RFC3986, section 4.2
--
--   relative-URI  = relative-part [ "?" query ] [ "#" fragment ]
--
--   relative-part = "//" authority path-abempty
--                 / path-abs
--                 / path-noscheme
--                 / path-empty
*)
val relative_partP : (cauth option * string list) parser =
  (stringP "//" |-- Scan.catch (authorityP -- path_abemptyP)) (* FIXME committed choice *)
    || (path_absP >> (fn up => (NONE, up)))
    || (path_no_schemeP >> (fn up => (NONE, up)))
    || Scan.succeed (NONE, [])

val relative_referenceP : curi parser =
  not_matching schemeP |-- relative_partP -- queryP -- fragmentP
    >> (fn (((ua, up), uq), uf) => { scheme = ""
                                 , authority = ua
                                 , path = up
                                 , query = uq
                                 , fragment = uf
                                 })

(*
--  RFC3986, section 4.1
*)

val referenceP : curi parser =
  uriP || relative_referenceP

(*
--  RFC3986, section 4.3
*)

val absoluteP : curi parser = (* FIXME uri suffix, not prefix in name ??? *) (* FIXME decode *)
  schemeP -- hier_partP -- queryP
    >> (fn ((us, (ua, up)), uq) => { scheme = us
                                 , authority = ua
                                 , path = up
                                 , query = uq
                                 , fragment = "" (* FIXME verify *)
       })

(* RFC7230 section 5.3.1 -- origin-form *)

val origin_formP : curi parser =
  path_absP -- queryP
    >> (fn (up, uq) => { scheme = "" (* FIXME *)
                     , authority = NONE (* FIXME *)
                     , path = up
                     , query = uq
                     , fragment = ""
       })

(*
------------------------------------------------------------
-- Resolving a relative URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the value of the
--  first 'URI' interpreted as relative to the second 'URI'.
--  For example:
--
--  > "foo" `relativeTo` "http://bar.org/" = "http://bar.org/foo"
--  > "http:foo" `nonStrictRelativeTo` "http://bar.org/" = "http://bar.org/foo"
--
--  Algorithm from RFC3986 [3], section 5.2.2
--

nonStrictRelativeTo :: URI -> URI -> URI
nonStrictRelativeTo ref base = relativeTo ref' base
    where
        ref' = if scheme ref == scheme base
               then ref { scheme="" }
               else ref

isDefined :: ( MonadPlus m, Eq (m a) ) => m a -> Bool
isDefined a = a /= mzero

-- | Returns a new 'URI' which represents the value of the first 'URI'
-- interpreted as relative to the second 'URI'.
--
-- Algorithm from RFC3986 [3], section 5.2
relativeTo :: URI -> URI -> URI
relativeTo ref base
    | isDefined ( scheme ref ) =
        just_segments ref
    | isDefined ( authority ref ) =
        just_segments ref { scheme = scheme base }
    | isDefined ( path ref ) =
        if (head (path ref) == '/') then
            just_segments ref
                { scheme    = scheme base
                , authority = authority base
                }
        else
            just_segments ref
                { scheme    = scheme base
                , authority = authority base
                , path      = mergePaths base ref
                }
    | isDefined ( query ref ) =
        just_segments ref
            { scheme    = scheme base
            , authority = authority base
            , path      = path base
            }
    | otherwise =
        just_segments ref
            { scheme    = scheme base
            , authority = authority base
            , path      = path base
            , query     = query base
            }
    where
        just_segments u =
            u { path = removeDotSegments (path u) }
        mergePaths b r
            | isDefined (authority b) && null pb = '/':pr
            | otherwise                             = dropLast pb ++ pr
            where
                pb = path b
                pr = path r
        dropLast = fst . splitLast -- reverse . dropWhile (/='/') . reverse

--  Remove dot segments, but protect leading '/' character
removeDotSegments :: String -> String
removeDotSegments ('/':ps) = '/':elimDots ps []
removeDotSegments ps       = elimDots ps []

--  Second arg accumulates segments processed so far in reverse order
elimDots :: String -> [String] -> String
-- elimDots ps rs | traceVal "\nps " ps $ traceVal "rs " rs $ False = error ""
elimDots [] [] = ""
elimDots [] rs = concat (reverse rs)
elimDots (    '.':'/':ps)     rs = elimDots ps rs
elimDots (    '.':[]    )     rs = elimDots [] rs
elimDots (    '.':'.':'/':ps) rs = elimDots ps (drop 1 rs)
elimDots (    '.':'.':[]    ) rs = elimDots [] (drop 1 rs)
elimDots ps rs = elimDots ps1 (r:rs)
    where
        (r,ps1) = nextSegment ps

--  Returns the next segment and the rest of the path from a path string.
--  Each segment ends with the next '/' or the end of string.
--
nextSegment :: String -> (String,String)
nextSegment ps =
    case break (=='/') ps of
        (r,'/':ps1) -> (r++"/",ps1)
        (r,_)       -> (r,[])

--  Split last (name) segment from path, returning (path,name)
splitLast :: String -> (String,String)
splitLast p = (reverse revpath,reverse revname)
    where
        (revname,revpath) = break (=='/') $ reverse p

------------------------------------------------------------
-- Finding a URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the relative location of
--  the first 'URI' with respect to the second 'URI'.  Thus, the
--  values supplied are expected to be absolute URIs, and the result
--  returned may be a relative URI.
--
--  Example:
--
--  > "http://example.com/Root/sub1/name2#frag"
--  >   `relativeFrom` "http://example.com/Root/sub2/name2#frag"
--  >   == "../sub1/name2#frag"
--
--  There is no single correct implementation of this function,
--  but any acceptable implementation must satisfy the following:
--
--  > (uabs `relativeFrom` ubase) `relativeTo` ubase == uabs
--
--  For any valid absolute URI.
--  (cf. <http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html>
--       <http://lists.w3.org/Archives/Public/uri/2003Jan/0005.html>)
--
relativeFrom :: URI -> URI -> URI
relativeFrom uabs base
    | diff scheme    uabs base = uabs
    | diff authority uabs base = uabs { scheme = "" }
    | diff path      uabs base = uabs
        { scheme    = ""
        , authority = Nothing
        , path      = relPathFrom (removeBodyDotSegments $ path uabs)
                                     (removeBodyDotSegments $ path base)
        }
    | diff query     uabs base = uabs
        { scheme    = ""
        , authority = Nothing
        , path      = ""
        }
    | otherwise = uabs          -- Always carry fragment from uabs
        { scheme    = ""
        , authority = Nothing
        , path      = ""
        , query     = ""
        }
    where
        diff :: Eq b => (a -> b) -> a -> a -> Bool
        diff sel u1 u2 = sel u1 /= sel u2
        -- Remove dot segments except the final segment
        removeBodyDotSegments p = removeDotSegments p1 ++ p2
            where
                (p1,p2) = splitLast p

relPathFrom :: String -> String -> String
relPathFrom []   _    = "/"
relPathFrom pabs []   = pabs
relPathFrom pabs base =                 -- Construct a relative path segments
    if sa1 == sb1                       -- if the paths share a leading segment
        then if (sa1 == "/")            -- other than a leading '/'
            then if (sa2 == sb2)
                then relPathFrom1 ra2 rb2
                else pabs
            else relPathFrom1 ra1 rb1
        else pabs
    where
        (sa1,ra1) = nextSegment pabs
        (sb1,rb1) = nextSegment base
        (sa2,ra2) = nextSegment ra1
        (sb2,rb2) = nextSegment rb1

--  relPathFrom1 strips off trailing names from the supplied paths,
--  and calls difPathFrom to find the relative path from base to
--  target
relPathFrom1 :: String -> String -> String
relPathFrom1 pabs base = relName
    where
        (sa,na) = splitLast pabs
        (sb,nb) = splitLast base
        rp      = relSegsFrom sa sb
        relName = if null rp then
                      if (na == nb) then ""
                      else if protect na then "./"++na
                      else na
                  else
                      rp++na
        -- Precede name with some path if it is null or contains a ':'
        protect s = null s || ':' `elem` s

--  relSegsFrom discards any common leading segments from both paths,
--  then invokes difSegsFrom to calculate a relative path from the end
--  of the base path to the end of the target path.
--  The final name is handled separately, so this deals only with
--  "directory" segtments.
--
relSegsFrom :: String -> String -> String
{-
relSegsFrom sabs base
    | traceVal "\nrelSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
relSegsFrom []   []   = ""      -- paths are identical
relSegsFrom sabs base =
    if sa1 == sb1
        then relSegsFrom ra1 rb1
        else difSegsFrom sabs base
    where
        (sa1,ra1) = nextSegment sabs
        (sb1,rb1) = nextSegment base

--  difSegsFrom calculates a path difference from base to target,
--  not including the final name at the end of the path
--  (i.e. results always ends with '/')
--
--  This function operates under the invariant that the supplied
--  value of sabs is the desired path relative to the beginning of
--  base.  Thus, when base is empty, the desired path has been found.
--
difSegsFrom :: String -> String -> String
{-
difSegsFrom sabs base
    | traceVal "\ndifSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
difSegsFrom sabs ""   = sabs
difSegsFrom sabs base = difSegsFrom ("../"++sabs) (snd $ nextSegment base)
*)

(* FIXME cheap tests. See the Haskell lib's tests *)

(*
val google = "http://peteg@google.com:8080/stuff/more/?s=XXX#YYY"
(* val google = "http://peteg@127.0.0.1:8080/stuff/more/?s=XXX#YYY"
val google = "http://126.75.34.12/"
val google = "../../stuff" *)

val _ = case run_parser uriP google of
            NONE => print "NO PARSE\n"
          | SOME u => (print (to_string I u); print "\n")
*)

end
