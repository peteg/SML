(*

A simple-minded generic TCP server.

FIXME try to abstract from INetSock.stream_sock: should also work on UNIX domain sockets or whatever.
FIXME general treatment of exceptions arising from threads. See what Makarius does.

*)

signature GENERIC_SERVER =
sig

  (* FIXME expose more stuff. serve does not return. Allow this to be interrupted. *)
  val serve : int -> (Socket.active INetSock.stream_sock -> unit) -> 'a

end

functor Generic_server (thread : THREAD) :> GENERIC_SERVER =
struct

val name: string = "Generic_server"

(* Cheesy get-IP thing. *)
fun hostaddr () : NetHostDB.in_addr list =
    let val hostname = NetHostDB.getHostName ()
    in
      case NetHostDB.getByName hostname of
           NONE => raise Fail "Can't get hostname"
         | SOME name => NetHostDB.addrs name
    end

fun print_hostaddrs ips port : unit =
  let
    fun print_hostaddr ip =
      Log.log {name=name, level=Log.App}
              (fn () => "Server available at http://" ^ NetHostDB.toString ip ^ ":" ^ Int.toString port ^ "/")
  in
    List.app print_hostaddr ips
  end

fun accept_loop s handle_request : 'a =
  let
    val (s', client_addr) = Socket.accept s
    val (client_in_addr, client_port) = INetSock.fromAddr client_addr
    val client_str = NetHostDB.toString client_in_addr ^ ":" ^ Int.toString client_port
    fun handler () =
        let in
          handle_request s'
          handle exn =>
            let in
              Log.log {name=name, level=Log.Error}
                      (fn () => "client " ^ client_str ^ "exn: " ^ Platform.exnMessage exn)
              (* ; Platform.reraise exn *)
            end
        ; Socket.close s'
        end
  in
    Log.log {name=name, level=Log.Info}
            (fn () => "Got connection from client: " ^ client_str)
  ; thread.fork handler
  ; accept_loop s handle_request
  end

fun serve port handle_request : 'a =
  let
    val s = INetSock.TCP.socket ()
  in
    (* UnixSignals.setHandler (UnixSignals.sigPIPE, UnixSignals.IGNORE); FIXME *)
    Socket.Ctl.setREUSEADDR (s, true); (* http://stackoverflow.com/questions/14388706/socket-options-so-reuseaddr-and-so-reuseport-how-do-they-differ-do-they-mean-t *)
    Socket.bind (s, INetSock.any port); (* FIXME configure, by default probably want just localhost *)
    Socket.listen (s, 10);
    print_hostaddrs (hostaddr ()) port;
    accept_loop s handle_request
  end

end

(* FIXME can imagine using Posix.Process.fork too *)
structure Thread_synchronous : THREAD =
struct

fun fork f = f ()

end

structure Generic_server_synchronous = Generic_server(Thread_synchronous)
structure Generic_server_threaded = Generic_server(Platform.Thread)
