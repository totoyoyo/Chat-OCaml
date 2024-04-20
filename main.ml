(* open Unix *)
open Lwt
open ChatHelpers.Message
open ChatHelpers.Sender
open ChatHelpers.Receiver
open ChatHelpers.Promises
open ChatHelpers.Client
open ChatHelpers.Server


type mode = Server | Client

let usage_msg = "main [-m server|client] [-a <addr>] [-p <port>]"

let runningMode = ref Server
let addrStr = ref "127.0.0.1"
let port = ref 8888


(* let setMode mode  =
  if !runningMode <> Default then
    raise @@ Arg.Bad "Cannot use both modes, -c and -s."
else 
    runningMode := mode *)
(* 
let processArgs args =
  if List.length args <> 2 then
    raise @@ Arg.Help usage_msg
  else 
    match args with 
    | addr :: p :: [] -> 
      addrStr := addr;
      port := int_of_string p 
    | other -> failwith "Never" *)


let speclist =
  [("-m", Arg.Symbol (["server"; "client"], (fun s -> 
    if s = "client" then
      runningMode := Client 
    else if s = "server" then 
      runningMode := Server 
    else 
      raise @@ Arg.Bad "Invalid mode.")) , "Set running mode.");
   ("-a", Arg.Set_string addrStr , "Set connection address");
   ("-p", Arg.Set_int port, "Set connection port.")
   ]


let () = 
  Arg.parse speclist (fun _ -> ()) usage_msg;
  match !runningMode with 
  | Server -> Lwt_main.run (startserver !addrStr !port) 
  | Client -> Lwt_main.run (startclient !addrStr !port) 

  
(* let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <port>\n" Sys.argv.(0)
  else if Array.length Sys.argv = 2 then begin
    let port = int_of_string Sys.argv.(1) in
    Lwt_main.run (startserver "127.0.0.1" port) ;
  end 
  else if Array.length Sys.argv = 3 then begin
    let hostname = Sys.argv.(1) in
    let port = int_of_string Sys.argv.(2) in
    Lwt_main.run (startclient "127.0.0.1" port)
  end else
    Printf.printf "Usage: %s <port> or %s <hostname> <port>\n" Sys.argv.(0) Sys.argv.(0) *)