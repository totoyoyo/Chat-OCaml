open ChatHelpers.Client
open ChatHelpers.Server


type mode = Server | Client

let usage_msg = "main [-m server|client] [-a <addr>] [-p <port>]"
let runningMode = ref Server
let addrStr = ref "127.0.0.1"
let port = ref 8888


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
  | Server -> Lwt_main.run (startServer !addrStr !port) 
  | Client -> Lwt_main.run (startClient !addrStr !port) 

