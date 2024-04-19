open Lwt
open Message


let writeToChan (msg: message) chan errMsg =
  Lwt.catch (fun () -> 
    Lwt_io.write_value chan ~flags:[] msg )
    ( fun _ -> Lwt_io.printl errMsg)

let sendMessage (msg: string) (toSend: Lwt_io.output_channel) : unit Lwt.t = 
  (* Get time of send for future acknowledgement *)
  let currentTime = Unix.gettimeofday() in
  let newMessage : message = { t = SEND currentTime;
    data = msg
  } in

  (* Try writing to the out_channel*)
  writeToChan newMessage toSend "Write message failed."



(* Sending acknowledgement*)
let sendAck (f: float) str (toSend: Lwt_io.output_channel) : unit Lwt.t =
  (* Create a short string for acknowledgement*)
  let short = if (String.length str > 10) then
    String.cat (String.sub str 0 10) "..."
  else 
    str in
  let newMessage : message = { t = ACK f;
  data = short
  } in 

  writeToChan newMessage toSend "Write acknowledgement failed."


let sendStop (toSend: Lwt_io.output_channel) : unit Lwt.t = 
  (* Send a STOP message for disconnection*)
  let newMessage : message = { t = STOP;
  data = ""
  } in
  writeToChan newMessage toSend "Write STOP failed."

  
(* Promise with a send loop, reading user input from terminal *)
let handle_read_input (to_send) promisesList: unit t = 
  let rec looping () =
    Lwt_io.print "> " >>= fun () ->
    Lwt_io.read_line_opt Lwt_io.stdin >>= fun read ->
      match read with
      | Some strInside -> 
        if (String.length(strInside) < 1) then
          looping()
        else
          sendMessage strInside to_send >>= fun () ->
          looping ()
      | None -> 
        Lwt_io.printl "\n Lost connection." ;%lwt
        Lwt.return_unit
  in
  looping ()




