open Chat_helpers.Client
open Chat_helpers.Server


type mode = Server | Client

let usage_msg = Printf.sprintf "%s [-m {server|client}] [-a <addr>] [-p <port>]" Sys.argv.(0) 
let mode = ref Server
let addr_str = ref "127.0.0.1"
let port = ref 8888


let spec =
  [("-m", Arg.Symbol (["server"; "client"], (fun s -> 
    if s = "client" then
      mode := Client 
    else if s = "server" then 
      mode := Server 
    else 
      raise @@ Arg.Bad "Invalid mode.")) , " Set running mode.");
   ("-a", Arg.Set_string addr_str , "Set connection address.");
   ("-p", Arg.Set_int port, "Set connection port.")
   ]


let () = 
  Arg.parse spec (fun a -> 
    raise (Arg.Bad (Printf.sprintf "Invalid argument: %s" a))
    ) usage_msg;
  match !mode with 
  | Server -> Lwt_main.run (start_server !addr_str !port) 
  | Client -> Lwt_main.run (start_client !addr_str !port) 

