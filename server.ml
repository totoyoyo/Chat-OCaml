(* open Unix *)
open Lwt
open ChatLib.Message


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
      exn -> 
      Lwt_io.printl "\n Read Failed" ;%lwt
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
      let sending = List.hd !promisesList in
      Lwt.cancel sending;
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
        Lwt.return_unit
  in
  looping ()

  (* Handlers for signals termination, etc.*)
let register_handlers (output) promisesList =
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
      )))
  
    

let makePromises socket  =
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  let runningPromises = ref [] in
  let pReceive = handle_receiving in_channel out_channel runningPromises in
  let pSend = handle_read_input out_channel runningPromises in
  runningPromises :=  pSend :: pReceive :: !runningPromises;
  register_handlers(out_channel) runningPromises;
  runningPromises



let startclient name inputPort : unit t = 
  Printf.printf "Welcome. You are the client. \n";
  flush stdout;
  let client_socket = Lwt_unix.socket Unix.PF_INET SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", inputPort) in
  Lwt_unix.connect client_socket addr ;%lwt
  let promises = makePromises client_socket in



  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  let runningPromises = ref [] in
  let pReceive = handle_receiving in_channel out_channel runningPromises in
  let pSend = handle_read_input out_channel runningPromises in
  runningPromises :=  pSend :: pReceive :: !runningPromises;
  register_handlers(out_channel) runningPromises;
  Lwt.all !runningPromises  >>= fun _ ->
    Lwt_io.close in_channel >>= fun() -> Lwt_io.close out_channel

  (* Lwt.catch (fun () -> Lwt.join [handle_read_input out_channel; handle_receiving in_channel out_channel])
    (fun e -> Lwt.return_unit) *)

  
  
  
  (* Lwt.async (fun () -> handle_read_input out_channel);
  handle_receiving in_channel out_channel *)





let acceptAndHandle server_socket : unit t =
  let restarted = ref false in
  let rec keepAccepting () =
    (if !restarted 
    then Lwt_io.printl "Restarting connection. \n"
    else Lwt_io.printl "Accepting new connections. \n");%lwt
    Lwt_unix.accept server_socket >>= fun (connectSock, sockAddr) ->
      let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input connectSock in
      let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output connectSock in
      let runningPromises = ref [] in
      let pReceive = handle_receiving in_channel out_channel runningPromises in
      let pSend = handle_read_input out_channel runningPromises in
      runningPromises :=  pSend :: pReceive :: !runningPromises;
      register_handlers(out_channel) runningPromises;
      Lwt.finalize (fun () -> 
        Lwt.all !runningPromises) (fun _ -> 
          restarted := true;
          Lwt_unix.close connectSock;%lwt
          keepAccepting ()
          ) >>= 
          fun a ->
        return_unit

       (* >>= fun _ ->
        restarted := true;
        Lwt_unix.close connectSock;%lwt
        keepAccepting () *)
    in
    keepAccepting()



let startserver inputPort firstTime : unit t = 
  (if firstTime then Printf.printf "Welcome. You are the server!! \n"
  else Printf.printf "Sever restarting \n");
  flush stdout;
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let address =  Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", inputPort) in
  Lwt_unix.bind server_socket address >>= fun () -> 
  Lwt_unix.listen server_socket 1; 
  acceptAndHandle server_socket

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else if Array.length Sys.argv = 2 then begin
    let port = int_of_string Sys.argv.(1) in
    print_endline "Open as server";

    Lwt_main.run (startserver port true) ;

  end 
  else if Array.length Sys.argv = 3 then begin
    let hostname = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
    print_endline "Open as client";
    let toRun = Lwt.finalize (fun () -> startclient hostname port) (fun () ->
      Lwt_io.printl "Ending" >>= fun () -> Lwt_io.(flush stdout) 
      ) in
    Lwt_main.run (toRun)
  end else
    Printf.printf "Usage: %s <port> or %s <hostname> <port>\n" Sys.argv.(0) Sys.argv.(0)



(* let startserverMock input = 
  (* Printf.printf "Welcome to a One-on-one chat session \n";
  Printf.printf "Say something! \n";
  Printf.printf "> ";
  let inputText = read_line () in
  Printf.printf " %s \n" inputText; *)
  let address =  Unix.ADDR_INET (Unix.inet_addr_any, 8888) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind server_socket address >>= fun () -> Lwt_unix.listen server_socket 1; 
  let rec read_user_loop () : unit t =
    Lwt_io.printl "Please write something" >>= fun() ->
    Lwt_io.read_line (Lwt_io.stdin) >>= fun input_text ->
      (Printf.printf "User input: %s \n" input_text);
      read_user_loop ();
    in
  Lwt.async (fun () -> read_user_loop ());
  let rec read_user_loop2 () : unit t =
    Lwt_unix.sleep 5.0 >>= fun () ->
    Lwt_io.printl "I woke up. Sleeping again" >>= fun() ->
    read_user_loop2 ();
    in
  read_user_loop2 ()


let _ = Lwt_main.run (startserver ())

  
 *)


  (* let rec server_loop () = 
    Lwt_unix.accept server_socket >>= fun (client_socket, client_address) -> 
      let in_channel = Lwt_io.of_fd Lwt_io.input client_socket in
      let out_channel = Lwt_io.of_fd Lwt_io.output client_socket in
      Lwt.async(fun () -> handle_client (in_channel, out_channel));
      server_loop ()
  in server_loop(); *)

    



(* let () =  
  let commands = Sys.argv.(1) in
  () *)