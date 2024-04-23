open Lwt
open Sender 



let safeClose socket =
  if (
    try 
      print_endline "really";
      true
    with 
      _ -> false) then
  try%lwt 
    Lwt_unix.check_descriptor socket;
    Lwt_unix.close socket 
  with
  | Unix.Unix_error (e,code,fn) ->
    if e = Unix.EBADF then
      return_unit else 
    Lwt_io.printf " %s : %s\n" code fn
  else return_unit

  (* Undo the signal handlers. Ex. Ctrl+C should work after client already*)
let undoHandlers handlerList =
  List.iter (fun h-> Lwt_unix.disable_signal_handler h) handlerList

   (* Handlers for signals: Ctrl C, Terminal closing, etc.*)
let register_handlers output promisesList =
  let handlerList = ref [] in
  (* Generic function*)
  let handle_signals signal name =
    let out = Lwt_unix.on_signal signal  
      (fun _ ->
        (* Make a promise the cancels all other promises and stops the connection.*)
        Lwt.async (
          fun () -> 
            Lwt_io.printlf "Received a %s signal. " name ;%lwt
            (* Sends a STOP message to the other side*)
            (if not @@ Lwt_io.is_closed output 
              then sendStop output
            else
              return_unit );%lwt
            List.iter (fun p -> Lwt.cancel p) !promisesList;
            return_unit
            )) 
      in handlerList := out :: !handlerList

  in
  handle_signals Sys.sigterm "SIGTERM";
  handle_signals Sys.sigint "SIGINT";
  handle_signals Sys.sighup "SIGHUP";
  handle_signals Sys.sigpipe "SIGPIPE";
  !handlerList

let createSocketAddr addrStr port reuse =
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
