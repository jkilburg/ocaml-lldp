open! Core
open Async
open Netstubs

type t =
  { name   : string
  ; hw     : string
  ; fd     : Unix.Fd.t
  ; inbuf  : (read_write, Iobuf.seek) Iobuf.t
  ; outbuf : ((read_write, Iobuf.seek) Iobuf.t) option
  ; cond   : unit Condition.t
  } [@@deriving fields]

let udp_packet_size = 10000
  
(* Pick only interfaces with IP addresses *)
let pick' () =
  Unix.getifaddrs ()
  >>| fun ifaddrs ->
  List.filter_map ifaddrs
    ~f:(fun ifaddr ->
        match Unix.Ifaddr.family ifaddr with
        | Unix.Ifaddr.Family.Packet -> None
        | Unix.Ifaddr.Family.Inet4
        | Unix.Ifaddr.Family.Inet6  ->
          Some (Unix.Ifaddr.name ifaddr))
;;

let pick () =
  Deferred.Or_error.try_with (fun () -> pick' ())
;;

let read t =
  match Iobuf.read t.inbuf (Unix.Fd.file_descr_exn t.fd) with
  | Iobuf.Eof -> Deferred.Or_error.errorf "%s: unexpected EOF" t.name
  | Iobuf.Ok  ->
    Iobuf.rewind t.inbuf;
    Deferred.Or_error.return (Lldp.of_iobuf t.inbuf)
;;

let make_outbuf ~outgoing_ttl ~name ~hw () =
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
        ; Port_description   name
        ]
    }
;;

let write t =
  match t.outbuf with
  | None        -> Deferred.Or_error.errorf "This is a read-only interface"
  | Some outbuf ->
    Iobuf.rewind outbuf;
    (* CR jkilburg: at least log `Bad_fd and `Closed *)
    match%bind Unix.Fd.ready_to t.fd `Write with
    | `Bad_fd -> Deferred.Or_error.ok_unit
    | `Closed -> Deferred.Or_error.ok_unit
    | `Ready  ->
      Iobuf.write outbuf (Unix.Fd.file_descr_exn t.fd);
      Deferred.Or_error.ok_unit
;;

let setup_socket name =
  let open Core.Or_error in
  let error msg = function
    | Error ec  -> Or_error.errorf "%s Errno = %d" msg ec
    | Ok _ as z -> z
  in
  error "Packet.socket" (Packet.socket Lldp.protocol_number)
  >>= fun fd ->
  error "Netdevice.siocgifindex" (Netdevice.siocgifindex fd name)
  >>= fun ifindex ->
  error "Packet.bind" (Packet.bind fd ifindex Lldp.protocol_number)
  >>= fun () ->
  error "Netdevice.siocgifhwaddr" (Netdevice.siocgifhwaddr fd name)
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
  return (fd, hw)
;;

let create_t name =
  match setup_socket name with
  | Error _ as e -> return e
  | Ok (fd, hw)  ->
    let file_descr = Core.Unix.File_descr.of_int fd in
    let%bind kind = Unix.Fd.Kind.infer_using_stat file_descr in
    Deferred.Or_error.return
      { name
      ; hw
      ; fd     = Unix.Fd.create kind file_descr (Info.createf "")
      ; inbuf  = Iobuf.create ~len:udp_packet_size            
      ; outbuf = None
      ; cond   = Condition.create ()
      }
;;

let create ~outgoing_ttl ~transmit_interval name =
  create_t name
  >>=? fun t ->
  let t = { t with outbuf = Some (make_outbuf ~outgoing_ttl ~name ~hw:t.hw ()) } in
  (* Write LLDP every transmit_interval seconds *)
  every ~stop:(Condition.wait t.cond) transmit_interval
    (fun () ->
       write t
       >>> function
       | Ok ()   -> ()
       | Error e -> printf "Error while writing: %s\n" (Error.to_string_hum e));
  Deferred.Or_error.return t
;;

let create_read_only = create_t

let close t =
  Deferred.Or_error.try_with
    (fun () ->
       Condition.signal t.cond ();
       Unix.Fd.close t.fd)
;;
