open Lwt
open Message
open Sender

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
      | exn -> 
        Lwt_io.printl "\nFailed to receive message, likely due to disconnection." ;%lwt
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