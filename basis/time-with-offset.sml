(* Times with explicit timezones.
 *
 * Times are stored as seconds since the UNIX epoch: 00:00:00
 * Coordinated Universal Time (UTC), Thursday, 1 January 1970.
 *
 * Offsets (timezones) are stored separately as seconds from UTC,
 * following the SML Standard Basis.
 *
 * UNIX time is weird: http://en.wikipedia.org/wiki/Unix_time
 * ... so the exact semantics is not clear.
 *
 * Note that Date.date is a bit coarse: only down to seconds.
 *
 * FIXME take a look at https://blogs.janestreet.com/core-gems-time/
 *)

signature TIME_WITH_OFFSET =
sig
  type t
       = { time : Time.time
         , offset : Time.time }
  val to_date : t -> Date.date
  val to_string : t -> string
  val compare : t * t -> order
end

structure Time_with_offset :> TIME_WITH_OFFSET =
struct

open Time

type t
     = { time : Time.time
       , offset : Time.time }

fun to_date (t : t) : Date.date =
  Date.fromTimeUniv (#time t - #offset t)

fun to_string (t : t) : string =
  let val offset_to_string = Date.fmt "%H:%M" o Date.fromTimeUniv
      fun offset_with_sign_to_string x =
        if x < Time.zeroTime
        then "-" ^ offset_to_string (Time.zeroTime - x)
        else "+" ^ offset_to_string x
  in
    Date.fmt "%Y-%m-%dT%H:%M:%S" (to_date t)
    ^ offset_with_sign_to_string (#offset t)
  end

fun compare ((t0, t1) : t * t) : order =
    case Time.compare (#time t0, #time t1) of
        EQUAL => Time.compare (#offset t0, #offset t1) (* FIXME perhaps backwards *)
      | other => other

end
