open Core.Std
open Async.Std

let buflen = 10 * 1024

let recv fd buf =
  In_thread.run ~name:"lldp"
    (fun () -> Core.Std.Unix.recv fd ~buf ~pos:0 ~len:buflen ~mode:[])
;;

let rec read_all_lldp fd buf =
  recv fd buf
  >>= function
  | 0 -> Deferred.Or_error.error_string "Unexpected zero length packet"
  | res when res < 0 -> Deferred.Or_error.errorf "recv err %d" res
  | res ->
    printf "TLV:\n";
    let lldp = Lldp.of_iobuf (Iobuf.of_string buf) in
    printf "%s\n" (Sexp.to_string lldp);
    Deferred.Or_error.ok_unit
;;

let nearest_bridge = [ 0x01; 0x80; 0xc2; 0x00; 0x00; 0x0e ]
let nearest_nontpmr_bridge = [ 0x01; 0x80; 0xc2; 0x00; 0x00; 0x03 ]
let nearest_customer_bridge = [ 0x01; 0x80; 0xc2; 0x00; 0x00; 0x00 ]
    
let make_mac l =
  String.of_char_list (List.map l ~f:Char.of_int_exn)
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
  error "Packet.add_membership" (Packet.add_membership fd ifindex (make_mac nearest_bridge))
  >>= fun () ->
  error "Packet.add_membership" (Packet.add_membership fd ifindex (make_mac nearest_nontpmr_bridge))
  >>= fun () ->
  error "Packet.add_membership" (Packet.add_membership fd ifindex (make_mac nearest_customer_bridge))
  >>= fun () ->
  printf "Interface MAC: ";
  String.iter hw ~f:(fun c -> printf "%02x" (Char.to_int c));
  printf "\n";
  return fd
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
       | Ok fd ->
         let buf = Bytes.create buflen in
         read_all_lldp (Core.Std.Unix.File_descr.of_int fd) buf)
;;

let () = Command.run (main ())
