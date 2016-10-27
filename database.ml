open! Core.Std
open Async.Std

module Destination = struct
  type t = Stdout
         | Nowhere
         | Collector of Unix.Inet_addr.t * int
         | Filename of string

  let of_string s =
    let parse_fail () =
      failwith "Destination should be one of \
                collector:hostname:port,stdout,filename:path_of_filename,nowhere"
    in
    let s = String.lowercase s in
    if String.is_prefix ~prefix:"collector" s
    then
      let (ip,port) =
        match String.split ~on:':' s with
        | [_; hostname_or_ip; port] ->
          begin
            let module Unix = Core.Std.Unix in
            match
              Unix.getaddrinfo hostname_or_ip port [Unix.AI_FAMILY Unix.PF_INET]
            with
            | []          -> failwith "Destination parse error: unknown hostname"
            | addrinfo::_ ->               
              match addrinfo.Unix.ai_addr with
              | Unix.ADDR_UNIX _s        -> parse_fail ()
              | Unix.ADDR_INET (ip,port) -> (ip,port)
          end
        | _                   -> parse_fail ()
      in
      Collector (ip,port)
    else if String.(s = "stdout")
    then Stdout
    else if String.is_prefix ~prefix:"filename" s
    then
      Filename
        (match String.split ~on:':' s with
         | [_; filename] -> filename
         | _             -> parse_fail ())
    else if String.(s = "nowhere")
    then Nowhere
    else parse_fail ()
  ;;
end

type t =
  { sequencer    : unit Throttle.Sequencer.t
  ; incoming_ttl : Time.Span.t
  ; dest_info    : [ `Collector of Unix.Fd.t * (Core.Std.read_write, Iobuf.seek) Iobuf.t
                   | `Nowhere
                   | `Stdout
                   | `Filename of string
                   ]
  }

let create ~destination ~incoming_ttl () =
  let%map dest_info =
    match destination with
    | Destination.Filename filename     -> return (`Filename filename)
    | Destination.Nowhere               -> return `Nowhere
    | Destination.Stdout                -> return `Stdout
    | Destination.Collector (addr,port) ->
      let socket = Unix.Socket.create Unix.Socket.Type.udp in
      Unix.Socket.bind socket (`Inet (addr,port))
      >>| fun socket ->
      let fd = Unix.Socket.fd socket in
      let iobuf = Iobuf.create ~len:Async_extra.Udp.default_capacity in
      `Collector (fd,iobuf)
  in
  let sequencer = Throttle.Sequencer.create () in
  { sequencer
  ; incoming_ttl
  ; dest_info
  }
;;

type meta =
  { tlv   : Lldp.Tlv.t
  ; ttl   : Time.Span.t
  ; stamp : Time.t
  } [@@deriving sexp]

let empty () : meta list = []

let tlv_stash = ref (empty ())

(* Call only when sequenced *)
let write_db t =
  match t.dest_info with
  | `Nowhere              -> Deferred.unit
  | `Stdout               -> printf !"%{sexp:meta list}\n" !tlv_stash; Deferred.unit
  | `Filename f           -> Writer.save_sexp f ([%sexp_of: meta list] !tlv_stash)
  | `Collector (fd,iobuf) ->
    Iobuf.rewind iobuf;
    List.iter !tlv_stash
      ~f:(fun meta -> Iobuf.Fill.bin_prot Lldp.Tlv.bin_writer_t iobuf meta.tlv);
    printf "%s\n" (Iobuf.to_string_hum iobuf);
    Iobuf.rewind iobuf;
    Iobuf.write iobuf (Unix.Fd.file_descr_exn fd);
    Deferred.unit
;;

(* Call only when sequenced *)
let clean_aux t =
  let now = Time.now () in
  tlv_stash := 
    List.filter !tlv_stash
      ~f:(fun tlv_meta ->
          let lifetime = Time.diff now tlv_meta.stamp in
          Time.Span.(<) lifetime tlv_meta.ttl)
;;

let clean t =
  Throttle.enqueue t.sequencer (fun () -> clean_aux t; Deferred.unit)
;;

let add t lldp =
  (* Find the TTL TLV. If not found use the default. *)
  let ttl =
    match
      List.find_map (Lldp.tlvs lldp)
        ~f:(function
            | Lldp.Tlv.Ttl ttl -> Some ttl
            | _                -> None)
    with
    | None     -> t.incoming_ttl
    | Some ttl -> Time.Span.of_sec (Float.of_int ttl)
  in
  Throttle.enqueue t.sequencer
    (fun () ->
       (* Remove expired TLVs *)
       clean_aux t;
       List.iter (Lldp.tlvs lldp)
         ~f:(function
             | Lldp.Tlv.Ttl _ -> ()
             | new_tlv        ->
               (* Before adding a new record remove duplicates from the stash *)
               tlv_stash := List.filter !tlv_stash
                   ~f:(fun meta -> Lldp.Tlv.compare new_tlv meta.tlv <> 0);
               (* Add the TLV to the stash *)
               tlv_stash := { tlv = new_tlv; ttl; stamp = Time.now () }::(!tlv_stash));
       (* Write the database *)
       write_db t)
;;

let get _t = List.map !tlv_stash ~f:(fun meta -> meta.tlv) ;;
