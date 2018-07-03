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

(*  Title:      Pure/General/socket_io.ML
    Author:     Timothy Bourke, NICTA
    Author:     Makarius

Stream IO over TCP sockets.  Following example 10.2 in "The Standard
ML Basis Library" by Emden R. Gansner and John H. Reppy.

Note: BinIO requires Poly/ML 5.5.x to work reliably.
*)

signature SOCKET_IO =
sig
  val make_streams: Socket.active INetSock.stream_sock -> BinIO.instream * BinIO.outstream
  val open_streams: string -> BinIO.instream * BinIO.outstream
end;

structure Socket_IO: SOCKET_IO =
struct

open Basic_Library

val io_buffer_size = 4096;

fun make_streams socket =
  let
    val (host, port) = INetSock.fromAddr (Socket.Ctl.getSockName socket);
    val name = NetHostDB.toString host ^ ":" ^ string_of_int port;

    val rd =
      BinPrimIO.RD {
        name = name,
        chunkSize = io_buffer_size,
        readVec = SOME (fn n => Socket.recvVec (socket, n)),
        readArr = SOME (fn buffer => Socket.recvArr (socket, buffer)),
        readVecNB = NONE,
        readArrNB = NONE,
        block = NONE,
        canInput = NONE,
        avail = fn () => NONE,
        getPos = NONE,
        setPos = NONE,
        endPos = NONE,
        verifyPos = NONE,
        close = fn () => Socket.close socket,
        ioDesc = SOME (Socket.ioDesc socket)
      };

    val wr =
      BinPrimIO.WR {
        name = name,
        chunkSize = io_buffer_size,
        writeVec = SOME (fn buffer => Socket.sendVec (socket, buffer)),
        writeArr = SOME (fn buffer => Socket.sendArr (socket, buffer)),
        writeVecNB = NONE,
        writeArrNB = NONE,
        block = NONE,
        canOutput = NONE,
        getPos = NONE,
        setPos = NONE,
        endPos = NONE,
        verifyPos = NONE,
        close = fn () => Socket.close socket,
        ioDesc = SOME (Socket.ioDesc socket)
      };

    val in_stream =
      BinIO.mkInstream
        (BinIO.StreamIO.mkInstream (rd, Word8Vector.fromList []));

    val out_stream =
      BinIO.mkOutstream
        (BinIO.StreamIO.mkOutstream (wr, IO.BLOCK_BUF));

  in (in_stream, out_stream) end;


fun open_streams socket_name =
  let
    fun err () = error ("Bad socket name: " ^ quote socket_name);
    val (host, port) =
      (case space_explode ":" socket_name of
        [h, p] =>
         (case NetHostDB.getByName h of SOME host => host | NONE => err (),
          case Int.fromString p of SOME port => port | NONE => err ())
      | _ => err ());
    val socket: Socket.active INetSock.stream_sock = INetSock.TCP.socket ();
    val _ = Socket.connect (socket, INetSock.toAddr (NetHostDB.addr host, port));
    val _ = INetSock.TCP.setNODELAY (socket, true);
  in make_streams socket end;

end;

