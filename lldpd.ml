open Core.Std
open Async.Std

let buflen = 10 * 1024

let recv fd buf =
  In_thread.run ~name:"lldp-recv"
    (fun () -> Core.Std.Unix.recv fd ~buf ~pos:0 ~len:buflen ~mode:[])
;;

let send fd hw =
  let iobuf =
    let open Lldp in
    to_iobuf
      { destination_mac = Mac_address.nearest_bridge ()
      ; source_mac      = Mac_address.of_string hw
      ; tlvs            =
          [ Tlv.Chassis_id  (Tlv.Chassis_id_data.Local "myhostname")
          ; Tlv.Port_id     (Tlv.Port_id_data.Interface_name "eth0")
          ; Tlv.Ttl         300
          ; Tlv.System_name (Unix.gethostname ())
          ; Tlv.System_description "Some kind of computer"
          ; Tlv.Port_id
              (Tlv.Port_id_data.Mac_address (Mac_address.of_string hw))
          ]
      }
  in
  In_thread.run ~name:"lldp-send"
    (fun () -> Or_error.try_with (fun () -> Iobuf.write iobuf fd))
;;

let rec read_all_lldp fd buf =
  recv fd buf
  >>= function
  | 0 -> Deferred.Or_error.error_string "Unexpected zero length packet"
  | res when res < 0 -> Deferred.Or_error.errorf "recv err %d" res
  | res ->
    printf "TLV:\n";
    let lldp = Lldp.of_iobuf (Iobuf.of_string buf) in
    printf "%s\n" (Lldp.sexp_of_t lldp |> Sexp.to_string);
    Deferred.Or_error.ok_unit
;;

let setup_socket interface =
  let protocol_number = Lldp.lldp_protocol_number in
  let open Core.Std.Or_error in
  let error msg = function
    | Error ec  -> Or_error.errorf "%s Errno = %d" msg ec
    | Ok _ as z -> z
  in
  error "Packet.socket" (Packet.socket protocol_number)
  >>= fun fd ->
  error "Netdevice.siocgifindex" (Netdevice.siocgifindex fd interface)
  >>= fun ifindex ->
  error "Packet.bind" (Packet.bind fd ifindex protocol_number)
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
  printf "Interface MAC: ";
  String.iter hw ~f:(fun c -> printf "%02x" (Char.to_int c));
  printf "\n";
  return (fd,hw)
;;

let main () =
  Command.async_or_error
    ~summary:"Print packets"
    Command.Spec.(
      empty
      +> anon ( "INTERFACE" %: string)
    )
    (fun interface () ->
       match setup_socket interface with
       | Error _ as e -> return e
       | Ok (fd,hw) ->
         let fd = Core.Std.Unix.File_descr.of_int fd in
         let buf = Bytes.create buflen in
         read_all_lldp fd buf
         >>=? fun () ->
         send fd hw)
;;

let () = Command.run (main ())
