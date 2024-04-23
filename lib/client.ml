open Lwt
open Promises
open Utils



let startClient addrStr inputPort : unit t = 
  Lwt_io.printl "Welcome. You are the client.";%lwt
  let client_socket, addr = createSocketAddr addrStr inputPort false in
  Lwt_io.printlf "Attempting to connect to %s." (stringOfSockAddr addr); %lwt
  let%lwt continue = safeConnect client_socket addr in
  if not (continue) then return_unit else 
  Lwt_io.printl "Connected."; %lwt
  let promises, _ = makePromisesAndHandlers client_socket in
  Lwt.finalize (promises) 
    (fun () -> 
      safeClose client_socket;%lwt
      Lwt_io.printl "Shutting down client."
      )
