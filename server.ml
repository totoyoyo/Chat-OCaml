(* open Unix *)
open Lwt
open ChatHelpers.Message
open ChatHelpers.Sender


let writeToChan (msg: message) chan errMsg =
  Lwt.catch (fun () -> 
    Lwt_io.write_value chan ~flags:[] msg )
    ( fun exn -> Lwt_io.printl errMsg)



let sendMessage (msg: string) (toSend: Lwt_io.output_channel) : unit Lwt.t = 
  (* Get time of send for future acknowledgement *)
  let currentTime = Unix.gettimeofday() in
  let newMessage : message = { t = SEND currentTime;
    data = msg
  } in

  (* Try writing to the out_channel*)
  writeToChan newMessage toSend "Write message failed."



(* Sending acknowledgement*)
let sendAck (f: float) str (toSend: Lwt_io.output_channel) : unit Lwt.t =
  (* Create a short string for acknowledgement*)
  let short = if (String.length str > 10) then
    String.cat (String.sub str 0 10) "..."
  else 
    str in
  let newMessage : message = { t = ACK f;
  data = short
  } in 

  writeToChan newMessage toSend "Write acknowledgement failed."


let sendStop (toSend: Lwt_io.output_channel) : unit Lwt.t = 
  (* Send a STOP message for disconnection*)
  let newMessage : message = { t = STOP;
  data = ""
  } in
  writeToChan newMessage toSend "Write STOP failed."


(* Receive a message*)
let receiveMessage (toRec: Lwt_io.input_channel) toSend : bool Lwt.t =
  try%lwt 
  Lwt_io.read_value toRec >>= fun (msg: message) ->
    match msg.t with 
    | STOP -> 
      Lwt_io.printl "\n------ Disconnected ------" ;%lwt
      return_false
    | SEND f ->
      (* print_endline "Received SENT"; *)
      Lwt_io.printf "\n< %s \n> "  msg.data ;%lwt 
      sendAck f msg.data toSend;%lwt 
      return_true
    | ACK f -> 
      let currtime = Unix.gettimeofday() in
      let timeElasped = currtime -. f in
      Lwt_io.printf "\n    << Messaged received \"%s\". Roundtrip time: %f \n> " 
        msg.data timeElasped;%lwt
      return_true
  with 
      | Lwt.Canceled -> 
        return_false
      | End_of_file -> 
        return_false
      | exn -> 
        let errorStr = Printexc.to_string exn in 
        Lwt_io.printf "\nConnection stopped due to %s" errorStr ;%lwt
        return_false

      
(* Promise with a receive loop *)
let handle_receiving toRec toSend (promisesList: 'a t list ref): unit t = 
  let rec receiving () =
    (* If did not receive a stop or an exn, keep receiving from connection*)
    let%lwt continue = receiveMessage toRec toSend in
    if continue then receiving ()
    else 
      (* Else stop, and cancel the other promise (sender)*)
      (
      (* let sending = List.hd !promisesList in
      Lwt.cancel sending; *)
      List.iter (fun p -> Lwt.cancel p) !promisesList;
      return_unit
      )
  in
  receiving ()
    

(* Promise with a send loop, reading user input from terminal *)
let handle_read_input (to_send) promisesList: unit t = 
  let rec looping () =
    Lwt_io.print "> " >>= fun () ->
    Lwt_io.read_line_opt Lwt_io.stdin >>= fun read ->
      match read with
      | Some strInside -> 
        if (String.length(strInside) < 1) then
          looping()
        else
          sendMessage strInside to_send >>= fun () ->
          looping ()
      | None -> 
        Lwt_io.printl "\n Lost connection." ;%lwt
        List.iter (fun p -> Lwt.cancel p) !promisesList;
        Lwt.return_unit
  in
  looping ()

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
(* 

  ignore (Lwt_unix.on_signal Sys.sigterm (fun _ -> 
    Lwt.async (fun () -> sendStop output);
    failwith "Got a SIGTERM"
    ));
  ignore (Lwt_unix.on_signal Sys.sigint (fun _ -> 
    Lwt.async (fun () -> sendStop output);
    failwith "Got a SIGINT"
    ));
  ignore (Lwt_unix.on_signal Sys.sighup (fun _ -> 
      Lwt.async (fun () -> sendStop output);
      failwith "Got a SIGHUP"
      ));
  ignore (Lwt_unix.on_signal Sys.sigpipe (fun _ -> 
    Lwt.async (fun () -> 
      sendStop output;%lwt 
      List.iter (fun p -> Lwt.cancel p) !promisesList;
      Lwt_io.printl "Handled a SIGPIPE (write error)."
      ))) *)
  
    

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

(* 
  runningPromises *)



let startclient addrStr inputPort : unit t = 
  Lwt_io.printl "Welcome. You are the client.";%lwt
  let client_socket = Lwt_unix.socket Unix.PF_INET SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string addrStr, inputPort) in
  let%lwt continue = (try%lwt 
    Lwt_unix.connect client_socket addr;%lwt
    Lwt.return_true
  with 
    | exn  -> 
      Lwt_io.printf "Connection refused. %s:%d might not be available.\n" addrStr inputPort;%lwt
      Lwt_io.printl "Shutting down client.";%lwt
      return_false
      ) in
  if not continue then return_unit else 
  let promises = makePromises [client_socket] in
  Lwt.finalize (promises) 
    (fun () -> 
      Lwt_unix.close client_socket;%lwt
      Lwt_io.printl "Shutting down client."
      )
  (* Lwt.catch (fun () -> Lwt.join [handle_read_input out_channel; handle_receiving in_channel out_channel])
    (fun e -> Lwt.return_unit) *)

  
  
  
  (* Lwt.async (fun () -> handle_read_input out_channel);
  handle_receiving in_channel out_channel *)





let acceptAndHandle server_socket : unit t =
  let restarted = ref false in
  let rec keepAccepting () =
    (if !restarted 
    then Lwt_io.printl "Restarting connection."
    else Lwt_io.printl "Accepting new connections.");%lwt
    let%lwt connectSock, _ = Lwt_unix.accept server_socket in
    let promises = makePromises [connectSock; server_socket] in
    Lwt.finalize (promises)
      (fun _ -> 
        restarted := true;
        Lwt_unix.close connectSock;%lwt
        keepAccepting ()
        )
  in
  keepAccepting()



let startserver addrStr inputPort : unit t = 
  Lwt_io.printl "Welcome. You are the server.";%lwt
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let address =  Unix.ADDR_INET (Unix.inet_addr_of_string addrStr, inputPort) in
  Lwt_unix.bind server_socket address;%lwt
  Lwt_unix.listen server_socket 1; 
  Lwt.finalize (fun () -> acceptAndHandle server_socket) 
    (fun () -> Lwt_unix.close server_socket)


let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else if Array.length Sys.argv = 2 then begin
    let port = int_of_string Sys.argv.(1) in
    Lwt_main.run (startserver "127.0.0.1" port) ;
  end 
  else if Array.length Sys.argv = 3 then begin
    let hostname = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
    Lwt_main.run (startclient "127.0.0.1" port)
  end else
    Printf.printf "Usage: %s <port> or %s <hostname> <port>\n" Sys.argv.(0) Sys.argv.(0)