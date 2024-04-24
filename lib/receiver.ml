open Lwt
open Message
open Sender


(* Receive a message*)
let receive_message (toRec: Lwt_io.input_channel) toSend : bool Lwt.t =
  try%lwt 
  Lwt_io.read_value toRec >>= fun (msg: message) ->
    match msg.t with 
    | STOP -> 
      Lwt_io.printl "\n------ Disconnected ------" ;%lwt
      return_false
    | SEND f ->
      Lwt_io.printf "\n< %s \n> "  msg.data ;%lwt 
      send_ack f msg.data toSend;%lwt 
      return_true
    | ACK f -> 
      let curr_time = Unix.gettimeofday() in
      let time_elasped = curr_time -. f in
      Lwt_io.printf "\n    << Messaged received \"%s\". Roundtrip time: %f \n> " 
        msg.data time_elasped;%lwt
      return_true
  with 
      | Lwt.Canceled -> 
        return_false
      | End_of_file -> 
        return_false
      | exn -> 
        let err_str = Printexc.to_string exn in 
        Lwt_io.printf "\nConnection stopped due to %s" err_str ;%lwt
        return_false

