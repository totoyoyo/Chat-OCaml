open Lwt
open Sender 


(* Translate socket address to string*)
let string_of_sock_addr sock_addr = 
  match sock_addr with
  | Unix.ADDR_UNIX s -> s 
  | ADDR_INET (inet_addr, port) -> 
    Unix.string_of_inet_addr inet_addr ^ ":" ^ string_of_int port


(* Close a file descriptor and handle errors*)
let safe_close socket =
  try%lwt 
    Lwt_unix.check_descriptor socket;
    Lwt_unix.close socket 
  with
  | Unix.Unix_error (e,code,fn) ->
    if e = Unix.EBADF then
      return_unit else 
    Lwt_io.printf " %s : %s\n" code fn

(* Connect a client-side to some server address*)
let safe_connect client_socket addr = 
  try%lwt 
    Lwt_unix.connect client_socket addr;%lwt
    Lwt.return_true
  with 
    | _  -> 
      Lwt_io.printf "Connection refused. %s might not be available.\n"  (string_of_sock_addr addr);%lwt
      Lwt_io.printl "Shutting down client.";%lwt
      return_false
    
(* Bind socket and handle errors*)
let safe_bind server_socket addr = 
  try%lwt 
    Lwt_unix.bind server_socket addr;%lwt
    Lwt.return_true
  with 
    | _  -> 
      Lwt_io.printf "Server binding refused. %s might not be available.\n"  (string_of_sock_addr addr);%lwt
      Lwt_io.printl "Shutting down server.";%lwt
      return_false
    
  
(* Undo the signal handlers. Ex. Ctrl+C should work after client already disconnected*)
let undo_handlers handlers =
  List.iter (fun h -> Lwt_unix.disable_signal_handler h) handlers

(* Handlers for signals: Ctrl C, Terminal closing, etc.*)
let register_handlers output promises =
  let handlers = ref [] in
  (* Generic function*)
  let handle_signals signal name =
    (*On receiving a signal, do ...*)
    let out = Lwt_unix.on_signal signal  
      (fun _ ->
        (* Make a promise the cancels all other promises and stops the connection.*)
        Lwt.async (
          fun () -> 
            Lwt_io.printlf "Received a %s signal. " name ;%lwt
            (* Sends a STOP message to the other side, if channel is still open*)
            (if not @@ Lwt_io.is_closed output 
              then send_stop output
            else
              return_unit );%lwt
            (* Cancel all promises*)
            List.iter (fun p -> Lwt.cancel p) !promises;
            return_unit
            )) 
      in handlers := out :: !handlers

  in
  handle_signals Sys.sigterm "SIGTERM";
  handle_signals Sys.sigint "SIGINT";
  handle_signals Sys.sighup "SIGHUP";
  handle_signals Sys.sigpipe "SIGPIPE";
  !handlers

(* Create a new socket and generate an address *)
let create_sock_addr addrStr port reuse =
  try 
    let socket = Lwt_unix.socket Unix.PF_INET SOCK_STREAM 0 in
    let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string addrStr, port) in
    (if reuse then Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true
    else ());
    socket, addr
  with
    | Unix.Unix_error (_, code, name) ->
      print_endline code ;
      print_endline name;
      raise Exit
    | Failure e ->
      print_string "Provided ";
      raise (Failure e)
