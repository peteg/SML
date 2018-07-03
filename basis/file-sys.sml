(* Extra filesystem stuff. *)

(*

Use SML Standard Basis OS.Path, for better or worse.
 - very dodgy semantics, see comments in Poly/ML's implementation.
Isabelle's path.ML is too opaque.

For better or worse, paths are apparently strings...

*)

signature FILE_SYS =
sig
  type path = { isAbs : bool, vol : string, arcs : filename list } (* from OS.Path *)

  val ls_dir : filename -> filename CoSeq.t (* FIXME arguable interface *)
  val find : { depth : int
             , follow : bool
             , roots : filename list
             } -> (filename * Posix.FileSys.ST.stat) CoSeq.t (* FIXME list of dirs? *)
end

structure FileSys :> FILE_SYS =
struct

type path = { isAbs : bool, vol : string, arcs : filename list }

structure FS = Posix.FileSys

fun ls_dir path : filename CoSeq.t =
  let
    val dirstream = FS.opendir path
    fun f () = Option.map (fn x => (x, ())) (FS.readdir dirstream)
  in
    CoSeq.unfold (f, ())
  end

(* A variant of http://ocamlunix.forge.ocamlcore.org/files.html *)
(* FIXME exceptions *)
(* FIXME path representation *)
(* FIXME depth should probably be optional *)
fun find {depth, follow, roots} =
    let
      fun f (depth: int) visiting (filename : filename) : (filename * FS.ST.stat) CoSeq.t =
        let
          val infos = (if follow then FS.stat else FS.lstat) filename
          val fid = (FS.ST.dev infos, FS.ST.ino infos)
        in
          if FS.ST.isDir infos andalso depth > 0
             andalso (not follow orelse not (List.mem visiting fid)) (* FIXME follow ?? looks unsound in the presence of hard links *)
          then
            let
              val visiting =
                  if follow then fid :: visiting else visiting (* FIXME follow ?? similarly, unsound *)
              open Basics (* FIXME *)
            in
              CoSeq.cons ( (filename, infos)
                         , CoSeq.bind
                             (ls_dir filename
                                |> CoSeq.map (fn f => OS.Path.joinDirFile {dir=filename, file=f}))
                             (f (depth - 1) visiting)
                         )
            end
          else CoSeq.return (filename, infos)
        end
    in
      CoSeq.bind (CoSeq.from_list roots) (f depth [])
    end

end
