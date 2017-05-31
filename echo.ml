open Core
open Lwt

let server_port =
  match Sys.getenv "SERVER_PORT" with
  | None -> 5000
  | Some s -> Int.of_string s

let backlog =
  match Sys.getenv "BACKLOG" with
  | None -> 10
  | Some s -> Int.of_string s

let try_close channel =
  catch (fun () -> Lwt_io.close channel)
    (function _ -> return ())

let initialize_socket address =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind socket address;
  Lwt_unix.listen socket backlog;
  socket

let process socket ~timeout ~callback =
  let rec loop () =
    Lwt_unix.accept socket >>=
    (fun (socket_fd, _) ->
       let in_c = Lwt_io.of_fd ~mode:Lwt_io.input socket_fd in
       let out_c = Lwt_io.of_fd ~mode:Lwt_io.output socket_fd in
       let c = callback in_c out_c in
       let events =
         match timeout with
         | None -> [c]
         | Some t -> [c; Lwt_unix.sleep (Float.of_int t) >>= (fun () -> return ())]
       in
       ignore (Lwt.pick events >>= (fun () -> try_close out_c) >>= (fun () -> try_close in_c));
       loop ()
    )
  in
  loop ()

let () =
  let server_address = Unix.ADDR_INET (Unix.Inet_addr.bind_any, server_port) in
  let socket = initialize_socket server_address in

  Lwt_main.run (
    process socket ~timeout:None ~callback: (fun in_c out_c ->
        Lwt_io.read_line in_c >>= (fun msg -> Lwt.join [
            Lwt_io.write_line Lwt_io.stdout msg;
            Lwt_io.write_line out_c msg
          ])
      )
  )

