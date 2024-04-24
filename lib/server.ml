open Lwt
open Promises
open Utils


let create_connection server_socket address =
  let%lwt continue = safe_bind server_socket address in
  if (not continue) then Lwt.return None else 
  begin
  Lwt_unix.listen server_socket 0;
  Lwt_io.printlf "Accepting connections at %s." (string_of_sock_addr address); %lwt
  (* Acception connection, and then close the listening socket to prevent new connections*)
  let%lwt connect_sock, _ = Lwt.finalize (fun () -> Lwt_unix.accept server_socket) 
    (fun () -> Lwt_unix.close server_socket) in
  Lwt_io.printl "Connected." ; %lwt
  Lwt.return (Some connect_sock)
  end

let start_server addr_str port : unit t =
    Lwt_io.printl "Welcome. You are the server.";%lwt
    let restarted = ref false in
    let rec keep_accepting () =
      (if !restarted then Lwt_io.printl "Restarting connection." else return_unit);%lwt
      let server_socket, address = create_sock_addr addr_str port true in
      let%lwt connect = create_connection server_socket address in
      match connect with 
      | None -> return_unit 
      | Some connect_sock ->
      let promises, handlers = make_promises_and_handlers connect_sock in
      Lwt.finalize (promises)
        (fun _ -> 
          restarted := true;
          safe_close connect_sock;%lwt
          undo_handlers handlers;
          keep_accepting ()
          )
    in
    keep_accepting()

