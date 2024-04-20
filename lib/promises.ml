open Lwt
open Sender
open Receiver

(* Handlers for signals: Ctrl C, Terminal closing, etc.*)
let register_handlers sockets output promisesList =
  (* Generic function*)
  let handle_signals signal name fail =
    ignore (Lwt_unix.on_signal signal  
      (fun _ ->
        (* Make a promise the cancels all other promises and stops the connection.*)
        Lwt.async (
          fun () -> 
            (* Sends a STOP message to the other side*)
            sendStop output;%lwt 
            (* Cancel all other promises*)
            List.iter (fun p -> Lwt.cancel p) !promisesList;
            (* If not failing, described the handled signal*)
            if (not fail) then 
              Lwt_io.printlf "Handled a %s signal. Continuing." name
            (* If failing, described the signal*)
            else 
              Lwt_list.iter_p (fun s -> Lwt_unix.close s) sockets;%lwt
              Lwt.fail_with (Printf.sprintf "Got a %s signal. Closing." name)
            )))
  in
  handle_signals Sys.sigterm "SIGTERM" true;
  handle_signals Sys.sigint "SIGINT" true;
  handle_signals Sys.sighup "SIGHUP" true;
  handle_signals Sys.sigpipe "SIGPIPE" false

let createSocketAddr addrStr port =
  try 
    let client_socket = Lwt_unix.socket Unix.PF_INET SOCK_STREAM 0 in
    let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string addrStr, port) in
    client_socket, addr
  with
    | Unix.Unix_error (e, code, name) ->
      print_endline code ;
      print_endline name;
      raise Exit
    | Failure e ->
      print_string "Provided ";
      raise e

let makePromises sockets  =
  let connectSocket = List.hd sockets in
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input connectSocket in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output connectSocket in
  let runningPromises = ref [] in
  let pReceive = handle_receiving in_channel out_channel runningPromises in
  let pSend = handle_read_input out_channel runningPromises in
  runningPromises :=  pSend :: pReceive :: !runningPromises;
  register_handlers sockets out_channel runningPromises;
  fun () -> 
    try%lwt 
      Lwt.all !runningPromises >>= fun _ -> return_unit
    with
    | Lwt.Canceled -> 
      Lwt_io.printl "\nDisconnected." 