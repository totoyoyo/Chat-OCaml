open Lwt
open Sender
open Receiver
open Utils


let makePromisesAndHandlers sockets  =
  let connectSocket = List.hd sockets in
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input connectSocket in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output connectSocket in
  let runningPromises = ref [] in
  let pReceive = handle_receiving in_channel out_channel runningPromises in
  let pSend = handle_read_input out_channel runningPromises in
  runningPromises :=  pSend :: pReceive :: !runningPromises;
  let handlers = register_handlers sockets out_channel runningPromises in
  (fun () -> 
    try%lwt 
      Lwt.all !runningPromises >>= fun _ -> return_unit
    with
    | Lwt.Canceled -> 
      Lwt_io.printl "\nDisconnected." ), handlers