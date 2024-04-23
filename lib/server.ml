open Lwt
open Promises
open Utils

let acceptAndHandle server_socket : unit t =
  let restarted = ref false in
  let rec keepAccepting () =
    (if !restarted 
    then 
      Lwt_io.printl "Restarting connection."
    else Lwt_io.printl "Accepting new connections.");%lwt
    let%lwt connectSock, _ = Lwt_unix.accept server_socket in
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

let acceptAndHandleMod addrStr inputPort : unit t =
    let restarted = ref false in
    let rec keepAccepting () =
      (if !restarted 
      then 
        Lwt_io.printl "Restarting connection."
      else Lwt_io.printl "Accepting new connections.");%lwt
      let server_socket, address = createSocketAddr addrStr inputPort true in
      Lwt_unix.bind server_socket address;%lwt
      Lwt_unix.listen server_socket 0; 
      let%lwt connectSock, _ = Lwt_unix.accept server_socket in
      Lwt_unix.close server_socket;%lwt
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

let startserver addrStr inputPort : unit t = 
  Lwt_io.printl "Welcome. You are the server.";%lwt
  let server_socket, address = createSocketAddr addrStr inputPort true in
  Lwt_unix.bind server_socket address;%lwt
  Lwt_unix.listen server_socket 0; 
  Lwt.finalize (fun () -> acceptAndHandle server_socket) 
    (fun () -> safeClose server_socket)

let startserverMod addrStr inputPort : unit t = 
  Lwt_io.printl "Welcome. You are the server.";%lwt
  acceptAndHandleMod addrStr inputPort
    