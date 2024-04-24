open Lwt
open Promises
open Utils



let start_client addr_str port : unit t = 
  Lwt_io.printl "Welcome. You are the client.";%lwt
  let client_socket, addr = create_sock_addr addr_str port false in
  Lwt_io.printlf "Attempting to connect to %s." (string_of_sock_addr addr); %lwt
  let%lwt continue = safe_connect client_socket addr in
  if not (continue) then return_unit else 
  Lwt_io.printl "Connected."; %lwt
  let promises, _ = make_promises_and_handlers client_socket in
  Lwt.finalize (promises) 
    (fun () -> 
      safe_close client_socket;%lwt
      Lwt_io.printl "Shutting down client."
      )
