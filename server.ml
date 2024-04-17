(* open Unix *)
open Lwt
open ChatLib.Message
(* 
let () = Printf.printf "Welcome to a One-on-one chat session \n"
let store = read_line ()  
let () = Printf.printf "> %s \n" store
let buffer_size = 2
let buffer = Bytes.create buffer_size *)

(* let file_copy input_name output_name =
  let fd_in = openfile input_name [O_RDONLY] 0 in
  let fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () = match read fd_in buffer 0 buffer_size with
    |  0 -> ()
    | r -> ignore (write fd_out buffer 0 r); copy_loop ()
  in
  copy_loop ();
  close fd_in;
  close fd_out

let copy () =
  if Array.length Sys.argv = 3 then 
  begin
    file_copy Sys.argv.(1) Sys.argv.(2);
    exit 0
  end else 
  begin
    prerr_endline
      ("Usage: " ^ Sys.argv.(0) ^ " <input_file> <output_file>");
    exit 1
  end  *)


let online = ref true

let (server_is_ready, notify_server_is_ready) = Lwt.wait ()


let sendMessage (msg: string) (toRec: Lwt_io.input_channel) 
    (toSend: Lwt_io.output_channel)  : unit Lwt.t = 
  let currentTime = Unix.gettimeofday() in
  let newMessage : message = { t = SEND currentTime;
    data = msg
  } in
  (* Lwt.catch (fun () -> Lwt_io.write_value toSend ~flags:[] newMessage) (fun exn) *)
  print_endline "Sending a SEND";
  if (Lwt_io.is_busy toSend) then print_endline "Out Chan is busy";
  if (Lwt_io.is_closed toSend) then print_endline "Out Chan is closed";
  if (Lwt_io.is_busy toRec) then print_endline "In Chan is busy";
  if (Lwt_io.is_closed toRec) then print_endline "In Chan is closed";
  flush stdout;
  Lwt.catch (fun () -> 
    Lwt_io.write_value toSend ~flags:[] newMessage >>= fun () ->
      Lwt_io.printl "Send ran successfully!."
    ) (fun exfn -> 
    Printf.printf "noway";
    flush stdout;
    Lwt_io.printl "Tried writing and failed"
    )
  
  



let sendAck (f: float) str (toSend: Lwt_io.output_channel) : unit Lwt.t =
  let short = if (String.length str > 10) then
    String.sub str 0 10
  else 
    str in
  let newMessage : message = { t = ACK f;
  data = short
  } in 
  print_endline "Sending ACK";
  Lwt_io.write_value toSend ~flags:[] newMessage


let receiveMessage (toRec: Lwt_io.input_channel) (toSend: Lwt_io.output_channel) : unit Lwt.t =
  Lwt_io.read_value toRec >>= fun (msg: message) ->
    print_endline "Received";
    flush_all;
    match msg.t with 
    | SEND f ->
      (* print_endline "Received SENT"; *)
      Lwt_io.printf "\n< %s \n> "  msg.data >>= fun () ->
        sendAck f msg.data toSend
    | ACK f -> 
      (* print_endline "Received ACK"; *)
      let currtime = Unix.gettimeofday() in
      let timeElasped = currtime -. f in
      Lwt_io.printf "\n    << Messaged received \"%s\" Roundtrip time: %f \n> " msg.data timeElasped

let handle_receiving toRec toSend: unit t = 
  let rec receiving () =
    receiveMessage toRec toSend >>= fun () ->
    (* if (Lwt_io.is_busy toRec) then print_endline "Handle: In Chan is close";
    if (Lwt_io.is_closed toRec) then print_endline "Handle: In Chan is close"; *)
    flush stdout;
    receiving ()
  in
  receiving ()

    
(* let register_handlers () =
  ignore (Lwt_unix.on_signal Sys.sigterm (fun _ -> failwith "Caught SIGTERM"));
  ignore (Lwt_unix.on_signal Sys.sigint (fun _ -> failwith "Caught SIGINT"));
  ignore (Lwt_unix.on_signal Sys.sigpipe (fun _ -> failwith "Caught SIGPIPE"))
 *)

let register_handlers (input, output) =
    ignore (Lwt_unix.on_signal Sys.sigterm (fun _ -> 
      Lwt.async (fun () -> 
        Lwt_io.close input;%lwt
        Lwt_io.close output
        );
      failwith "Caught SIGTERM")
      );
    ignore (Lwt_unix.on_signal Sys.sigint (fun _ -> 
      Lwt.async (fun () -> 
        Lwt_io.close input;%lwt
        Lwt_io.close output
        );
      failwith "Caught SIGINT"));
    ignore (Lwt_unix.on_signal Sys.sigpipe (fun _ -> 
      Lwt.async (fun () -> 
        Lwt_io.close input;%lwt
        Lwt_io.close output
        );
      failwith "Caught SIGPIPE"))
  


let handle_read_input to_rec (to_send) : unit t = 
  let rec looping () =
    Lwt_io.print "> " >>= fun () ->
    (* let line_lists = Lwt_io.read_lines Lwt_io.stdin |>
    Lwt_stream.get_available in
    let combined = List.fold_left (fun acc elem -> acc ^ elem ) "" line_lists in
      sendMessage combined to_send >>= fun () ->
      (* Lwt_unix.sleep 0.01 >>= fun () -> *)
      looping () *)
    Printf.printf "Trying to read\n";
    flush stdout;
    Lwt_io.read_line_opt Lwt_io.stdin >>= fun read ->
      Printf.printf "Just read something\n";
      match read with
      | Some strInside -> 
        if (String.length(strInside) < 1) then
          looping()
        else
          (Printf.printf "Read an input to send: %s \n" strInside ;
          sendMessage strInside to_rec to_send >>= fun () ->
          (* Lwt_unix.sleep 0.01 >>= fun () -> *)
          looping ())
      | None -> 
        Lwt_io.print "\n Lost connection." >>= fun () -> 
        flush_all ();
        Lwt.return_unit
  in
  looping ()

(* 
  let () =
  let p_1 =
    let () = Lwt_unix.sleep 3. in
    Lwt_io.printl "Three seconds elapsed"
  in

  let p_2 =
    let () = Lwt_unix.sleep 5. in
    Lwt_io.printl "Five seconds elapsed"
  in

  let p_3 = Lwt.join [p_1; p_2] in
  Lwt_main.run p_3 *)



let handleClient (input, output) = 
  register_handlers (input, output);
  Lwt.join [handle_read_input input output; handle_receiving input output]

let startclient name inputPort : unit t = 

  Printf.printf "Welcome. You are the client. \n";
  flush stdout;
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", inputPort) in
  Lwt_io.with_connection addr handleClient


(* 
  let client_socket = Lwt_unix.socket Unix.PF_INET SOCK_STREAM 0 in

  Lwt_unix.connect client_socket addr >>= fun () ->
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in 


  Lwt.join [handle_read_input out_channel; handle_receiving in_channel out_channel]  >>= fun () ->
    Lwt_io.close in_channel >>= fun() -> Lwt_io.close out_channel *)

  (* Lwt.catch (fun () -> Lwt.join [handle_read_input out_channel; handle_receiving in_channel out_channel])
    (fun e -> Lwt.return_unit) *)

  
  
  
  (* Lwt.async (fun () -> handle_read_input out_channel);
  handle_receiving in_channel out_channel *)




let handle_server _ (input,output) = 
  register_handlers (input, output);
  Lwt.join [handle_read_input input output; handle_receiving input output] 


let startserver inputPort = 
  Printf.printf "Welcome. You are the server!! \n";
  let address =  Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", inputPort) in
  let%lwt server = Lwt_io.establish_server_with_client_address address handle_server ~no_close:true in
  Lwt_io.printf "Listening for client connection on UNIX socket : \n" ;%lwt
  (* while !online do 
    ()
  done ; *)
  let%lwt _ = server_is_ready in
  Lwt_io.shutdown_server server
    







(* 
  Lwt_unix.accept server_socket >>= fun (connectSock, sockAddr) ->
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input connectSock in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output connectSock in
  Lwt.catch (fun () -> Lwt.join [handle_read_input out_channel; handle_receiving in_channel out_channel])
    (fun e -> Lwt.return_unit) >>= fun () ->
      Lwt.return_unit *)


  (* Lwt.join [handle_read_input out_channel; handle_receiving in_channel out_channel] *)
    (* Lwt.async (fun () -> handle_read_input out_channel);
  handle_receiving in_channel out_channel *)




let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else if Array.length Sys.argv = 2 then begin
    let port = int_of_string Sys.argv.(1) in
    print_endline "Open as server";
    let toRun = Lwt.finalize (fun () -> startserver port) (fun () ->
      Lwt_io.printl "Ending" >>= fun () -> Lwt_io.(flush stdout) 
      ) in
    Lwt_main.run (toRun) ;
    print_endline "Restarting";
    Lwt_main.run (toRun) ;
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