open! Core.Std
open Async.Std

let setup_socket interface =
  let open Core.Std.Or_error in
  let error msg = function
    | Error ec  -> Or_error.errorf "%s Errno = %d" msg ec
    | Ok _ as z -> z
  in
  error "Packet.socket" (Packet.socket Lldp.protocol_number)
  >>= fun fd ->
  error "Netdevice.siocgifindex" (Netdevice.siocgifindex fd interface)
  >>= fun ifindex ->
  error "Packet.bind" (Packet.bind fd ifindex Lldp.protocol_number)
  >>= fun () ->
  error "Netdevice.siocgifhwaddr" (Netdevice.siocgifhwaddr fd interface)
  >>= fun hw ->
  let mac = Lldp.Mac_address.(nearest_bridge () |> to_string) in
  error "Packet.add_membership" (Packet.add_membership fd ifindex mac)
  >>= fun () ->
  let mac = Lldp.Mac_address.(nearest_nontpmr_bridge () |> to_string) in
  error "Packet.add_membership" (Packet.add_membership fd ifindex mac)
  >>= fun () ->
  let mac = Lldp.Mac_address.(nearest_customer_bridge () |> to_string) in
  error "Packet.add_membership" (Packet.add_membership fd ifindex mac)
  >>= fun () ->
  return (fd,hw)
;;

let reader db fd =
  let inbound_iobuf = Iobuf.create ~len:10000 in
  let rec loop () =
    In_thread.run ~name:"lldp"
      (fun () -> Or_error.try_with (fun () -> Iobuf.read inbound_iobuf fd))
    >>=? function
    | Iobuf.Eof -> Deferred.Or_error.errorf "Unexpected EOF"
    | Iobuf.Ok  ->
      Iobuf.rewind inbound_iobuf;
      Database.add db (Lldp.of_iobuf inbound_iobuf)
      >>= fun ()->
      loop ()
  in
  loop ()
;;

let make_outbound_iobuf ~outgoing_ttl ~interface ~hw =
  let open Lldp in
  let mac = Mac_address.of_string hw in
  to_iobuf
    { destination_mac = Mac_address.nearest_bridge ()
    ; source_mac      = mac
    ; tlvs            =
        let open Tlv in
        [ Chassis_id         (Chassis_id_data.Local (Unix.gethostname ()))
        ; Port_id            (Port_id_data.Mac_address mac)
        ; Ttl                (Time.Span.to_sec outgoing_ttl |> Float.to_int)
        ; System_name        (Unix.gethostname ())
        ; System_description "Some kind of computer"
        ; Port_description   interface
        ]
    }
;;

let write ~outbound_iobuf fd =
  Iobuf.rewind outbound_iobuf;
  In_thread.run ~name:"lldp"
    (fun () -> Or_error.try_with (fun () -> Iobuf.write outbound_iobuf fd))
;;

let main ~outgoing_ttl ~incoming_ttl ~clean_interval ~interface () =
  match setup_socket interface with
  | Error _ as e -> return e
  | Ok (fd,hw)   ->
    let fd = Core.Std.Unix.File_descr.of_int fd in
    let outbound_iobuf = make_outbound_iobuf ~outgoing_ttl ~interface ~hw in
    (* Write one LLDP right away *)
    write ~outbound_iobuf fd
    >>=? fun () ->
    let db = Database.create ~incoming_ttl () in
    (* Write LLDP every outgoing_ttl seconds *)
    every outgoing_ttl
      (fun () ->
         write ~outbound_iobuf fd
         >>> function
         | Ok ()   -> ()
         | Error e ->
           printf "Error while writing: %s\n" (Error.to_string_hum e));
    (* Clean the database of incoming LLDP packets every clean_interval *)
    every clean_interval
      (fun () -> Database.clean db >>> fun () -> ());
    (* Dump the database *)
    every (Time.Span.of_sec 30.)
      (fun () -> printf !"%{sexp:Lldp.t list}\n" (Database.get db));
    (* Hopefully never returns from reading incoming packets *)
    reader db fd
;;

let () =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Read and write LLDP packets"
    [%map_open
      let outgoing_ttl =
        flag "-outgoing-ttl"
          (optional_with_default (Time.Span.of_sec 300.0) time_span)
          ~doc:"TIME-SPAN time-to-live for use in transmitted LLDP packets"
      and incoming_ttl =
        flag "-incoming-ttl"
          (optional_with_default (Time.Span.of_sec 300.0) time_span)
          ~doc:"TIME-SPAN time-to-live to apply to incoming packets \
                without a TTL TLV"
      and clean_interval =
        flag "-clean-interval"
          (optional_with_default (Time.Span.of_sec 1.0) time_span)
          ~doc:"TIME-SPAN cleanup LLDP database at this interval"
      and
        interface = anon ( "ETHERNET-INTERFACE-NAME" %: string )
      in
      main ~outgoing_ttl ~incoming_ttl ~clean_interval ~interface
    ]
  |> Command.run
;;
