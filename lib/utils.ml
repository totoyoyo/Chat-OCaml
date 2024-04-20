open Lwt
open Sender 



let safeClose socket =
  if (
    try 
      print_endline "really";
      true
    with 
      e -> false) then
  try%lwt 
    Lwt_unix.check_descriptor socket;
    Lwt_unix.close socket 
  with
  | Unix.Unix_error (e,code,fn) ->
    if e = Unix.EBADF then
      return_unit else 
    Lwt_io.printf " %s : %s\n" code fn
  | e -> 
    Lwt_io.printf " Something failed lol";%lwt
    Lwt_io.(flush stdout)
  else return_unit

  (* Undo the signal handlers. Ex. Ctrl+C should work after client already*)
let undoHandlers handlerList =
  List.iter (fun h-> Lwt_unix.disable_signal_handler h) handlerList

   (* Handlers for signals: Ctrl C, Terminal closing, etc.*)
let register_handlers sockets output promisesList =
  let handlerList = ref [] in
  (* Generic function*)
  let handle_signals signal name fail =
    let out = Lwt_unix.on_signal signal  
      (fun _ ->
        (* Make a promise the cancels all other promises and stops the connection.*)
        Lwt.async (
          fun () -> 
            (* Sends a STOP message to the other side*)
            (if not @@ Lwt_io.is_closed output 
              then sendStop output
            else
              return_unit );%lwt

            Lwt_io.printl "Sent stop";%lwt
            Lwt_io.(flush stdout);%lwt
            (* Cancel all other promises*)
            List.iter (fun p -> Lwt.cancel p) !promisesList;
            Lwt_io.printl "Cencelled promises";%lwt
            Lwt_io.(flush stdout);%lwt
            (* If not failing, described the handled signal*)
            if (not fail) then 
              Lwt_io.printlf "Handled a %s signal. Continuing." name
            (* If failing, described the signal*)
            else 
              (* Lwt_io.printl "BRUH" ;%lwt *)
              (* Lwt_io.printl "About to close sockets";%lwt
              Lwt_io.(flush stdout);%lwt
              Lwt_list.iter_s (fun s -> safeClose s) sockets;%lwt
              Lwt_io.printl "Closed sockets";%lwt
              Lwt_io.(flush stdout);%lwt *)
              Lwt_io.printf "Got a %s signal. Closing." name
            )) 
      in handlerList := out :: !handlerList

  in
  handle_signals Sys.sigterm "SIGTERM" false;
  handle_signals Sys.sigint "SIGINT" false;
  handle_signals Sys.sighup "SIGHUP" false;
  handle_signals Sys.sigpipe "SIGPIPE" false;
  !handlerList

let createSocketAddr addrStr port =
  try 
    let socket = Lwt_unix.socket Unix.PF_INET SOCK_STREAM 0 in
    let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string addrStr, port) in
    Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
    socket, addr
  with
    | Unix.Unix_error (e, code, name) ->
      print_endline code ;
      print_endline name;
      raise Exit
    | Failure e ->
      print_string "Provided ";
      raise (Failure e)
