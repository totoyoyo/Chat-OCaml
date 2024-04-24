open Message

(*Write to channel*)
let write_to_chan (msg: message) chan err_msg =
  try%lwt 
  Lwt_io.write_value chan ~flags:[] msg
  with 
  | _ -> Lwt_io.printl err_msg
  
(* Sends message*)
let send_message (msg: string) (to_send: Lwt_io.output_channel) : unit Lwt.t = 
  (* Get time of send for future acknowledgement *)
  let current_time = Unix.gettimeofday() in
  let new_message : message = { t = SEND current_time;
    data = msg
  } in

  (* Try writing to the out_channel*)
  write_to_chan new_message to_send "Write message failed."



(* Sending acknowledgement*)
let send_ack (f: float) str (to_send: Lwt_io.output_channel) : unit Lwt.t =
  (* Create a short string for acknowledgement*)
  let short = if (String.length str > 10) then
    String.cat (String.sub str 0 10) "..."
  else 
    str in
  let new_message : message = { t = ACK f;
  data = short
  } in 

  write_to_chan new_message to_send "Write acknowledgement failed."

(* Sends stop message before disconnecting*)
let send_stop (to_send: Lwt_io.output_channel) : unit Lwt.t = 
  (* Send a STOP message for disconnection*)
  let new_message : message = { t = STOP;
  data = ""
  } in
  write_to_chan new_message to_send "Write STOP failed."


