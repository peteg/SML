(*
ISABELLE COPYRIGHT NOTICE, LICENCE AND DISCLAIMER.

Copyright (c) 1986-2014,
  University of Cambridge,
  Technische Universitaet Muenchen,
  and contributors.

  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

* Neither the name of the University of Cambridge or the Technische
Universitaet Muenchen nor the names of their contributors may be used
to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(*  Title:      Pure/General/sha1.ML
    Author:     Makarius
    Author:     Sascha Boehme, TU Muenchen

Digesting strings according to SHA-1 (see RFC 3174).
*)

signature SHA1 =
sig
  eqtype digest
  val rep: digest -> string
  val fake: string -> digest
  val digest: string -> digest
  (* val test_samples: unit -> unit *)
end;

structure SHA1 :> SHA1 =
struct

(** internal implementation in ML -- relatively slow **)

local

(* 32bit words *)

infix 4 << >>;
infix 3 andb;
infix 2 orb xorb;

val op << = Word32.<<;
val op >> = Word32.>>;
val op andb = Word32.andb;
val op orb = Word32.orb;
val op xorb = Word32.xorb;
val notb = Word32.notb;

fun rotate k w = w << k orb w >> (0w32 - k);


(* hexadecimal words *)

(* FIXME this generates ASCII; we want binary. Rejig API to support both.
fun hex_digit (text, w: Word32.word) =
  let
    val d = Word32.toInt (w andb 0wxf);
    val dig = if d < 10 then chr (ord #"0" + d) else chr (ord #"a" + d - 10);
  in (str dig ^ text, w >> 0w4) end;

fun hex_word w = #1 (funpow 8 hex_digit ("", w));
*)

fun hex_digit (text, w: Word32.word) =
  let
    val d = Word32.toInt (w andb 0wxff);
    val dig = chr d;
  in (str dig ^ text, w >> 0w8) end;

fun hex_word w = #1 (Library.funpow 4 hex_digit ("", w));

(* padding *)

fun pack_bytes 0 _ = ""
  | pack_bytes k n = pack_bytes (k - 1) (n div 256) ^ str (chr (n mod 256));

fun padded_text s =
  let
    val len = size s;
    val padding = str (chr 128) ^ Library.replicate_string (~ (len + 9) mod 64) (str (chr 0)) ^ pack_bytes 8 (len * 8);
    fun byte i = Char.ord (String.sub (if i < len then (s, i) else (padding, (i - len))));
    fun word i =
      Word32.fromInt (byte (4 * i)) << 0w24 orb
      Word32.fromInt (byte (4 * i + 1)) << 0w16 orb
      Word32.fromInt (byte (4 * i + 2)) << 0w8 orb
      Word32.fromInt (byte (4 * i + 3));
  in ((len + size padding) div 4, word) end;


(* digest_string_internal *)

fun digest_word (i, w, {a, b, c, d, e}) =
  let
    val {f, k} =
      if i < 20 then
        {f = (b andb c) orb (notb b andb d),
         k = 0wx5A827999}
      else if i < 40 then
        {f = b xorb c xorb d,
         k = 0wx6ED9EBA1}
      else if i < 60 then
        {f = (b andb c) orb (b andb d) orb (c andb d),
         k = 0wx8F1BBCDC}
      else
        {f = b xorb c xorb d,
         k = 0wxCA62C1D6};
    val op + = Word32.+;
  in
    {a = rotate 0w5 a + f + e + w + k,
     b = a,
     c = rotate 0w30 b,
     d = c,
     e = d}
  end;

in

fun digest_string_internal str =
  let
    val (text_len, text) = padded_text str;

    (*hash result -- 5 words*)
    val hash_array : Word32.word Array.array =
      Array.fromList [0wx67452301, 0wxEFCDAB89, 0wx98BADCFE, 0wx10325476, 0wxC3D2E1F0];
    fun hash i = Array.sub (hash_array, i);
    fun add_hash x i = Array.update (hash_array, i, hash i + x);

    (*current chunk -- 80 words*)
    val chunk_array = Array.array (80, 0w0: Word32.word);
    fun chunk i = Array.sub (chunk_array, i);
    fun init_chunk pos =
      Array.modifyi (fn (i, _) =>
        if i < 16 then text (pos + i)
        else rotate 0w1 (chunk (i - 3) xorb chunk (i - 8) xorb chunk (i - 14) xorb chunk (i - 16)))
      chunk_array;

    fun digest_chunks pos =
      if pos < text_len then
        let
          val _ = init_chunk pos;
          val {a, b, c, d, e} = Array.foldli digest_word
            {a = hash 0,
             b = hash 1,
             c = hash 2,
             d = hash 3,
             e = hash 4}
            chunk_array;
          val _ = add_hash a 0;
          val _ = add_hash b 1;
          val _ = add_hash c 2;
          val _ = add_hash d 3;
          val _ = add_hash e 4;
        in digest_chunks (pos + 16) end
      else ();
    val _  = digest_chunks 0;

    val hex = hex_word o hash;
  in hex 0 ^ hex 1 ^ hex 2 ^ hex 3 ^ hex 4 end;

end;

(** external implementation in C **)

(*
local

(* digesting *)

fun hex_digit i = if i < 10 then chr (ord "0" + i) else chr (ord "a" + i - 10);

fun hex_string arr i =
  let val c = CInterface.fromCchar (CInterface.offset i CInterface.Cchar arr)
  in (op ^) (apply2 hex_digit (Integer.div_mod (Char.ord c) 16)) end

val lib_path =
  ("$ML_HOME/" ^ (if ML_System.platform_is_windows then "sha1.dll" else "libsha1.so"))
  |> Path.explode;

val STRING_INPUT_BYTES =
  CInterface.mkConversion undefined (CInterface.toCbytes o Byte.stringToBytes)
    (CInterface.Cpointer CInterface.Cchar);

in

fun digest_string_external str =
  let
    val digest = CInterface.alloc 20 CInterface.Cchar;
    val _ =
      CInterface.call3
        (CInterface.get_sym (File.platform_path lib_path) "sha1_buffer")
        (STRING_INPUT_BYTES, CInterface.LONG, CInterface.POINTER)
        CInterface.POINTER (str, size str, CInterface.address digest);
  in fold (suffix o hex_string digest) (0 upto 19) "" end;

end;
*)


(** type digest **)

datatype digest = Digest of string;

fun rep (Digest s) = s;
val fake = Digest;

(*
val _ = ML_system_pp (fn _ => fn _ => Pretty.to_polyml o Pretty.str o quote o rep);

fun digest_string str = digest_string_external str
  handle CInterface.Foreign msg =>
    (warning (msg ^ "\nUsing slow ML implementation of SHA1.digest"); digest_string_internal str);
*)

fun digest_string str = digest_string_internal str;

val digest = Digest o digest_string;



(** SHA1 samples found in the wild **)

(* FIXME update against binary/ASCII API *)
(*
local

fun check (msg, key) =
  let val key' = rep (digest msg) in
    if key = key' then ()
    else
      raise Fail ("SHA1 library integrity test failed on " ^ quote msg ^ ":\n" ^
        key ^ " expected, but\n" ^ key' ^ " was found")
  end;

in

fun test_samples () =
  List.app check
   [("", "da39a3ee5e6b4b0d3255bfef95601890afd80709"),
    ("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"),
    ("abc", "a9993e364706816aba3e25717850c26c9cd0d89d"),
    ("abcdefghijklmnopqrstuvwxyz", "32d10c7b8cf96570ca04ce37f2a19d84240d3a89"),
    ("The quick brown fox jumps over the lazy dog", "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"),
    (replicate_string 100 "\000", "ed4a77d1b56a118938788fc53037759b6c501e3d"),
    ("a\000b", "4a3dec2d1f8245280855c42db0ee4239f917fdb8"),
    ("\000\001", "3f29546453678b855931c174a97d6c0894b8f546")];

end;

val _ = test_samples ();
*)

end;
