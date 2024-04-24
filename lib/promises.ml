open Lwt
open Sender
open Receiver
open Utils

(* A promise/thread that reads terminal input and sends the input *)
let handle_read_input (to_send) promises: unit t = 
  let rec looping () =
    Lwt_io.print "> " >>= fun () ->
      (* Reads user input*)
    Lwt_io.read_line_opt Lwt_io.stdin >>= fun read ->
      match read with
      | Some str -> 
        (* If input is empty, keep looping, dont send*)
        if (String.length(str) < 1) then
          looping()
        else
          (* else send*)
          send_message str to_send >>= fun () ->
          looping ()
      | None -> 
        Lwt_io.printl "\n Lost connection." ;%lwt
        List.iter (fun p -> Lwt.cancel p) !promises;
        Lwt.return_unit
  in
  looping ()


(* Promise with a receive loop *)
let handle_receiving to_receive to_send (promises: 'a t list ref): unit t = 
  let rec receiving () =
    (* If did not receive a stop or an exn, keep receiving from connection*)
    let%lwt continue = receive_message to_receive to_send in
    if continue then receiving ()
    else 
      (* Else stop, and cancel the other promise (sender)*)
      (
      List.iter (fun p -> Lwt.cancel p) !promises;
      return_unit
      )
  in
  receiving ()

(* Make sending/receiving threads and register signal handlers*)
let make_promises_and_handlers connect_sock  =
 (* Convert socker to in/out channels*)
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.input connect_sock in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.output connect_sock in
  (* Store running promises for future cancellation*)
  let running_promises = ref [] in
  let p_receive = handle_receiving in_channel out_channel running_promises in
  let p_send = handle_read_input out_channel running_promises in
  (* Store sending and receiving promises*)
  running_promises :=  p_send :: p_receive :: !running_promises;
  (* register signal handlers*)
  let handlers = register_handlers out_channel running_promises in
  (fun () -> 
    try%lwt 
      Lwt.all !running_promises >>= fun _ -> return_unit
    with
    | Lwt.Canceled -> 
      Lwt_io.printl "\nDisconnected." ), handlers