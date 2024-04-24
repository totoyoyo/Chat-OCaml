# Simple OCaml Chat
This is a simple one-on-one chat application which allows two parties, the server and client, to communicate with each other. The server serves at a specific socket address (IP address + port number), and the client can connect to that address to start the chat. 

The UI is simply the terminal interface. Users can type their message into the terminal and press `ENTER` to send (or type the newline `\n` character). For every message received, the receiver would send an acknowledgement to its sender with the message's roundtrip time. To exit the chat, users can close the terminal instance or input the `Ctrl+C` interrupt. If a disconnect occurs, the client will shut down and the server will restart, listening for connections from the same socket address.

The implementation involves 2 `lwt` promises on each side of the connection: 
- One promise waits for user input and sends the input as a message.
- One promise waits to receive incoming messages, displays them, and sends acknowledgements back.

Message encodings are handled with the `Marshal` library of `OCaml`.

## Dependencies
- OCaml >= `5.1.1`
- opam >= `2.0`

This program was written with OCaml version `5.1.1`, so preferably the user should have OCaml version `5` or above. Compatibility with older versions is not guaranteed. Dependencies are managed with opam version `2.0`, and we assume that the user has opam installed.

Additional dependencies (like `lwt` and `dune`) can be installed with the provided `Makefile` and/or `.opam` file. The commands to install them are given in the Quickstart section below.

## Quickstart
To install dependencies and build, `cd` to the project directory and then run
```
make all
```

This previous command should install all the dependencies and build the project with `dune`. To separately install the dependencies, run 
```
make deps
```

After the dependencies are installed, subsequent builds can be done with just 
```
make
```

To execute the program with the default settings (server mode, localhost, port 8888), run
```
  ./chat.exe 
```

The arguments for this executable are described below:
```
./chat.exe [-m {server|client}] [-a <addr>] [-p <port>]
 -m {server|client} Set running mode.
 -a Set connection address.
 -p Set connection port.
 -help Display this list of options
```

## Server Mode
The server starts by accepting connections at the specified `address:port` socket address. Once the client connects, the chat begins. If a disconnect occurs, usually through the client disconnecting or a `Ctrl+C` interrupt on the server side, the server will restart and listen to the same socket address for new connections. If another `Ctrl+C` interrupt arrives before a client connects, the server shuts down.

## Client Mode
The client connects to some `address:port` socket address to start the chat. Naturally, there must be a server accepting connections at the same socket address. If there isn't one, the client shuts down. The client can end the chat by simply closing the terminal or using the `Ctrl+C` interrupt. The client will not automatically restart, unlike the server.