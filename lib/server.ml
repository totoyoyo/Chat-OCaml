open Lwt
open Promises
open Utils


let createConnection server_socket address =
  let%lwt continue = safeBind server_socket address in
  if (not continue) then Lwt.return None else 
  begin
  Lwt_unix.listen server_socket 0; 
  (* Close the listening socket to prevent new connections*)
  Lwt_io.printlf "Accepting connections at %s." (stringOfSockAddr address); %lwt
  let%lwt connectSock, _ = Lwt.finalize (fun () -> Lwt_unix.accept server_socket) 
    (fun () -> Lwt_unix.close server_socket) in
  Lwt_io.printl "Connected." ; %lwt
  Lwt.return (Some connectSock)
  end

let startServer addrStr inputPort : unit t =
    Lwt_io.printl "Welcome. You are the server.";%lwt
    let restarted = ref false in
    let rec keepAccepting () =
      (if !restarted then Lwt_io.printl "Restarting connection." else return_unit);%lwt
      let server_socket, address = createSocketAddr addrStr inputPort true in
      let%lwt connectRes = createConnection server_socket address in
      match connectRes with 
      | None -> return_unit 
      | Some connectSock ->
      let promises, handlers = makePromisesAndHandlers connectSock in
      Lwt.finalize (promises)
        (fun _ -> 
          restarted := true;
          safeClose connectSock;%lwt
          undoHandlers handlers;
          keepAccepting ()
          )
    in
    keepAccepting()

(* let startserver addrStr inputPort : unit t = 
  Lwt_io.printl "Welcome. You are the server.";%lwt
  let server_socket, address = createSocketAddr addrStr inputPort true in
  Lwt_unix.bind server_socket address;%lwt
  Lwt_unix.listen server_socket 0; 
  Lwt.finalize (fun () -> acceptAndHandle server_socket) 
    (fun () -> safeClose server_socket) *)

