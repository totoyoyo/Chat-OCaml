open Lwt
open Promises

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
  let server_socket, address = createSocketAddr addrStr inputPort in
  Lwt_unix.bind server_socket address;%lwt
  Lwt_unix.listen server_socket 1; 
  Lwt.finalize (fun () -> acceptAndHandle server_socket) 
    (fun () -> Lwt_unix.close server_socket)
