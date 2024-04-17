open Lwt

(* Define message type *)
(* type message = string *)

(* Function to send a message *)
let send_message out_channel msg =
  Lwt_io.write_line out_channel (msg ^ "haha") >>= fun () ->
  Lwt_io.flush out_channel >>= fun () ->
  Lwt.return ()

(* Function to receive a message *)
let receive_message in_channel =
  Lwt_io.read_line_opt in_channel >>= function
  | Some msg ->
      (* Lwt_io.write_line Lwt_io.stdout ("Message received: " ^ msg) >>= fun () ->
      Lwt_io.flush Lwt_io.stdout >>= fun () -> *)
      Lwt.return msg
  | None -> Lwt.fail End_of_file


let rec read_user_loop out_chan : unit t =
  Lwt_io.read_line (Lwt_io.stdin) >>= fun input_text ->
    (Printf.printf "User input: %s \n" input_text);
    send_message out_chan input_text >>= fun () ->
  read_user_loop out_chan


  (* Lwt_io.write_line out_chan input_text >>= fun () ->
  Lwt_io.flush out_chan >>= fun () ->
    read_user_loop out_chan *)
      


(* Function to handle client connection *)
let handle_client (in_channel, out_channel) =
  let rec loop () =
    receive_message in_channel >>= fun msg ->
    let start_time = Unix.gettimeofday () in
    send_message out_channel msg >>= fun () ->
    let end_time = Unix.gettimeofday () in
    let roundtrip_time = end_time -. start_time in
    Lwt_io.write_line Lwt_io.stdout ("Roundtrip time: " ^ string_of_float roundtrip_time) >>= fun () ->
    Lwt_io.flush Lwt_io.stdout >>= fun () ->
    loop ()
  in
  Lwt.catch loop (fun _ -> Lwt.return_unit)

(* Function to start the server *)
let start_server port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind server_socket sockaddr >>= fun () ->
  Lwt_unix.listen server_socket 5;
  let rec accept_loop () =
    Lwt_unix.accept server_socket >>= fun (client_socket, _) ->
    let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
    let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
    Lwt.async (fun () -> handle_client (in_channel, out_channel));
    read_user_loop out_channel
  in
  accept_loop ()

(* Function to start the client *)
let start_client hostname port =
  Lwt_unix.gethostbyname hostname >>= fun host_entry ->
  let sockaddr = Unix.ADDR_INET (host_entry.Unix.h_addr_list.(0), port) in
  let client_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect client_socket sockaddr >>= fun () ->
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  Lwt.async (fun () -> handle_client (in_channel, out_channel));
  read_user_loop out_channel

(* Entry point *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else if Array.length Sys.argv = 2 then begin
    let port = int_of_string Sys.argv.(1) in
    Lwt_main.run (start_server port)
  end else if Array.length Sys.argv = 3 then begin
    let hostname = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
    Lwt_main.run (start_client hostname port)
  end else
    Printf.printf "Usage: %s <port> or %s <hostname> <port>\n" Sys.argv.(0) Sys.argv.(0)