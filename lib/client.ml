open Lwt
open Promises
open Utils



let startclient addrStr inputPort : unit t = 
  Lwt_io.printl "Welcome. You are the client.";%lwt
  let client_socket, addr = createSocketAddr addrStr inputPort false in
(* 
  let client_socket = Lwt_unix.socket Unix.PF_INET SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string addrStr, inputPort) in *)
  let%lwt continue = (try%lwt 
    Lwt_unix.connect client_socket addr;%lwt
    Lwt.return_true
  with 
    | _  -> 
      Lwt_io.printf "Connection refused. %s:%d might not be available.\n" addrStr inputPort;%lwt
      Lwt_io.printl "Shutting down client.";%lwt
      return_false
      ) in
  if not continue then return_unit else 
  let promises, _ = makePromisesAndHandlers client_socket in
  Lwt.finalize (promises) 
    (fun () -> 
      safeClose client_socket;%lwt
      Lwt_io.printl "Shutting down client."
      )
