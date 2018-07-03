(* Various structures extracted from Isabelle. *)
(*
ISABELLE COPYRIGHT NOTICE, LICENCE AND DISCLAIMER.

Copyright (c) 1986-2018,
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

(*  Title:      Pure/Concurrent/multithreading.ML
    Author:     Makarius

Multithreading in Poly/ML (cf. polyml/basis/Thread.sml).
*)

(*  Title:      Pure/General/exn.ML
    Author:     Makarius

Support for exceptions.
*)

signature BASIC_EXN =
sig
  exception ERROR of string
  val error: string -> 'a
  val cat_error: string -> string -> 'a
end;

signature EXN =
sig
  include BASIC_EXN
  val polyml_location: exn -> PolyML.location option
  val reraise: exn -> 'a
  datatype 'a result = Res of 'a | Exn of exn
  val get_res: 'a result -> 'a option
  val get_exn: 'a result -> exn option
  val capture: ('a -> 'b) -> 'a -> 'b result
  val release: 'a result -> 'a
  val map_res: ('a -> 'b) -> 'a result -> 'b result
  val maps_res: ('a -> 'b result) -> 'a result -> 'b result
  val map_exn: (exn -> exn) -> 'a result -> 'a result
  exception Interrupt
  val interrupt: unit -> 'a
  val is_interrupt: exn -> bool
  val interrupt_exn: 'a result
  val is_interrupt_exn: 'a result -> bool
  val interruptible_capture: ('a -> 'b) -> 'a -> 'b result
  val return_code: exn -> int -> int
  exception EXCEPTIONS of exn list
  val trace: (exn -> string) -> (string -> unit) -> (unit -> 'a) -> 'a
end;

structure Exn: EXN =
struct

(* location *)

val polyml_location = PolyML.Exception.exceptionLocation;

val reraise = PolyML.Exception.reraise;


(* user errors *)

exception ERROR of string;

fun error msg = raise ERROR msg;

fun cat_error "" msg = error msg
  | cat_error msg "" = error msg
  | cat_error msg1 msg2 = error (msg1 ^ "\n" ^ msg2);


(* exceptions as values *)

datatype 'a result =
  Res of 'a |
  Exn of exn;

fun get_res (Res x) = SOME x
  | get_res _ = NONE;

fun get_exn (Exn exn) = SOME exn
  | get_exn _ = NONE;

fun capture f x = Res (f x) handle e => Exn e;

fun release (Res y) = y
  | release (Exn e) = reraise e;

fun map_res f (Res x) = Res (f x)
  | map_res f (Exn e) = Exn e;

fun maps_res f = (fn Res x => x | Exn e => Exn e) o map_res f;

fun map_exn f (Res x) = Res x
  | map_exn f (Exn e) = Exn (f e);


(* interrupts *)

exception Interrupt = Thread.Thread.Interrupt;

fun interrupt () = raise Interrupt;

fun is_interrupt Interrupt = true
  | is_interrupt (IO.Io {cause, ...}) = is_interrupt cause
  | is_interrupt _ = false;

val interrupt_exn = Exn Interrupt;

fun is_interrupt_exn (Exn exn) = is_interrupt exn
  | is_interrupt_exn _ = false;

fun interruptible_capture f x =
  Res (f x) handle e => if is_interrupt e then reraise e else Exn e;


(* POSIX return code *)

fun return_code exn rc =
  if is_interrupt exn then (130: int) else rc;


(* concatenated exceptions *)

exception EXCEPTIONS of exn list;


(* low-level trace *)

(* FIXME obsolete? *)
fun trace exn_message output e =
  PolyML.Exception.traceException
    (e, fn (trace, exn) =>
      let
        val title = "Exception trace - " ^ exn_message exn;
        val () = output (String.concatWith "\n" (title :: trace));
      in reraise exn end);

end;

(*  Title:      Pure/Concurrent/thread_attributes.ML
    Author:     Makarius

Thread attributes for interrupt handling.
*)

signature THREAD_ATTRIBUTES =
sig
  type attributes
  val get_attributes: unit -> attributes
  val set_attributes: attributes -> unit
  val convert_attributes: attributes -> Thread.Thread.threadAttribute list
  val no_interrupts: attributes
  val test_interrupts: attributes
  val public_interrupts: attributes
  val private_interrupts: attributes
  val sync_interrupts: attributes -> attributes
  val safe_interrupts: attributes -> attributes
  val with_attributes: attributes -> (attributes -> 'a) -> 'a
  val uninterruptible: ((('c -> 'd) -> 'c -> 'd) -> 'a -> 'b) -> 'a -> 'b
  val expose_interrupt: unit -> unit  (*exception Interrupt*)
end;

structure Thread_Attributes: THREAD_ATTRIBUTES =
struct

abstype attributes = Attributes of Word.word
with

(* thread attributes *)

val thread_attributes = 0w7;

val broadcast = 0w1;

val interrupt_state = 0w6;
val interrupt_defer = 0w0;
val interrupt_synch = 0w2;
val interrupt_asynch = 0w4;
val interrupt_asynch_once = 0w6;


(* access thread flags *)

val thread_flags = 0w1;

fun load_word () : word =
  RunCall.loadWord (RunCall.unsafeCast (Thread.Thread.self ()), thread_flags);

fun get_attributes () = Attributes (Word.andb (load_word (), thread_attributes));

fun set_attributes (Attributes w) =
  let
    val w' = Word.orb (Word.andb (load_word (), Word.notb thread_attributes), w);
    val _ = RunCall.storeWord (RunCall.unsafeCast (Thread.Thread.self ()), thread_flags, w');
  in
    if Word.andb (w', interrupt_asynch) = interrupt_asynch
    then Thread.Thread.testInterrupt () else ()
  end;

fun convert_attributes (Attributes w) =
  [Thread.Thread.EnableBroadcastInterrupt (Word.andb (w, broadcast) = broadcast),
   Thread.Thread.InterruptState
      let val w' = Word.andb (w, interrupt_state) in
        if w' = interrupt_defer then Thread.Thread.InterruptDefer
        else if w' = interrupt_synch then Thread.Thread.InterruptSynch
        else if w' = interrupt_asynch then Thread.Thread.InterruptAsynch
        else Thread.Thread.InterruptAsynchOnce
      end];


(* common configurations *)

val no_interrupts = Attributes interrupt_defer;
val test_interrupts = Attributes interrupt_synch;
val public_interrupts = Attributes (Word.orb (broadcast, interrupt_asynch_once));
val private_interrupts = Attributes interrupt_asynch_once;

fun sync_interrupts (Attributes w) =
  let
    val w' = Word.andb (w, interrupt_state);
    val w'' = Word.andb (w, Word.notb interrupt_state);
  in Attributes (if w' = interrupt_defer then w else Word.orb (w'', interrupt_synch)) end;

fun safe_interrupts (Attributes w) =
  let
    val w' = Word.andb (w, interrupt_state);
    val w'' = Word.andb (w, Word.notb interrupt_state);
  in Attributes (if w' = interrupt_asynch then Word.orb (w'', interrupt_asynch_once) else w) end;

fun with_attributes new_atts e =
  let
    val atts1 = safe_interrupts (get_attributes ());
    val atts2 = safe_interrupts new_atts;
  in
    if atts1 = atts2 then e atts1
    else
      let
        val result = Exn.capture (fn () => (set_attributes atts2; e atts1)) ();
        val _ = set_attributes atts1;
      in Exn.release result end
  end;

end;

fun uninterruptible f x =
  with_attributes no_interrupts (fn atts =>
    f (fn g => fn y => with_attributes atts (fn _ => g y)) x);

fun expose_interrupt () =
  let
    val orig_atts = safe_interrupts (get_attributes ());
    val _ = set_attributes test_interrupts;
    val test = Exn.capture Thread.Thread.testInterrupt ();
    val _ = set_attributes orig_atts;
  in Exn.release test end;

end;

signature MULTITHREADING =
sig
  val num_processors: unit -> int
  val sync_wait: Time.time option -> Thread.ConditionVar.conditionVar -> Thread.Mutex.mutex -> bool Exn.result
  val synchronized: string -> Thread.Mutex.mutex -> (unit -> 'a) -> 'a
end;

structure Multithreading: MULTITHREADING =
struct

local

open Thread

in

fun num_processors () =
  (case Thread.numPhysicalProcessors () of
    SOME n => n
  | NONE => Thread.numProcessors ());


(* synchronous wait *)

fun sync_wait time cond lock =
  Thread_Attributes.with_attributes
    (Thread_Attributes.sync_interrupts (Thread_Attributes.get_attributes ()))
    (fn _ =>
      (case time of
        SOME t => Exn.Res (ConditionVar.waitUntil (cond, lock, t))
      | NONE => (ConditionVar.wait (cond, lock); Exn.Res true))
      handle exn => Exn.Exn exn);


(* synchronized evaluation *)

fun synchronized name lock e =
  Exn.release (Thread_Attributes.uninterruptible (fn restore_attributes => fn () =>
      let
        val _ = Mutex.lock lock;
        val result = Exn.capture (restore_attributes e) ();
        val _ = Mutex.unlock lock;
      in result end) ());

end

end

(*  Title:      Pure/Concurrent/synchronized.ML
    Author:     Fabian Immler and Makarius

Synchronized variables.
*)

signature SYNCHRONIZED =
sig
  type 'a t
  val var: string -> 'a -> 'a t
  val value: 'a t -> 'a
  val timed_access: 'a t -> ('a -> Time.time option) -> ('a -> ('b * 'a) option) -> 'b option
  val guarded_access: 'a t -> ('a -> ('b * 'a) option) -> 'b
  val change_result: 'a t -> ('a -> 'b * 'a) -> 'b
  val change: 'a t -> ('a -> 'a) -> unit
end;

structure Synchronized :> SYNCHRONIZED =
struct

open Thread

datatype 'a t
  = Var of { name: string
           , lock: Mutex.mutex
           , cond: ConditionVar.conditionVar
           , var: 'a ref
           }

fun var name x = Var
 {name = name,
  lock = Mutex.mutex (),
  cond = ConditionVar.conditionVar (),
  var = ref x}

fun value (Var {name, lock, var, ...}) =
  Multithreading.synchronized name lock (fn () => ! var)

(* synchronized access *)

fun timed_access (Var {name, lock, cond, var}) time_limit f =
  Multithreading.synchronized name lock (fn () =>
    let
      fun try_change () =
        let val x = ! var in
          (case f x of
            NONE =>
              (case Multithreading.sync_wait (time_limit x) cond lock of
                Exn.Res true => try_change ()
              | Exn.Res false => NONE
              | Exn.Exn exn => Exn.reraise exn)
          | SOME (y, x') =>
              Thread_Attributes.uninterruptible (fn _ => fn () =>
                (var := x'; ConditionVar.broadcast cond; SOME y)) ())
        end;
    in try_change () end);

fun guarded_access var f =
    case timed_access var (fn _ => NONE) f of
        NONE => raise Fail "guarded_access got NONE"
      | SOME x => x

(* unconditional change *)

fun change_result var f = guarded_access var (SOME o f);
fun change var f = change_result var (fn x => ((), f x));

end
