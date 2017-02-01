open! Core.Std
open Async.Std

module Destination = struct
  type t = Stdout
         | Nowhere
         | Collector of Unix.Inet_addr.t * int
         | Filename of string

  let expressions = [ "collector:hostname:port"
                    ; "stdout"
                    ; "filename:path_of_filename"
                    ; "nowhere"
                    ]
                    
  let of_string s =
    let parse_fail () =
      failwithf "Destination should be one of %s" (String.concat ~sep:"," expressions) ()
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
      Unix.Socket.connect socket (`Inet (addr,port))
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

let tlv_stash = ref (String.Map.empty : meta list String.Map.t)

(* Call only when sequenced. *)
let write_db t ~interface =
  match t.dest_info with
  | `Nowhere              -> Deferred.unit
  | `Stdout               -> printf !"%{sexp:meta list String.Map.t}\n" !tlv_stash; Deferred.unit
  | `Filename f           -> Writer.save_sexp f ([%sexp_of: meta list String.Map.t] !tlv_stash)
  | `Collector (fd,iobuf) ->
    match Map.find !tlv_stash interface with
    | None           ->
      (* CR-soon jkilburg: log this? *)
      Deferred.unit
    | Some meta_list ->
      let tlvs = List.map meta_list ~f:(fun meta -> meta.tlv) in
      Site_server.send_packet ~fd ~interface ~tlvs iobuf
;;

(* Call only when sequenced *)
let clean_aux t =
  let now = Time.now () in
  tlv_stash := 
    Map.filter_map !tlv_stash
      ~f:(fun meta_list ->
          let meta_list =
            List.filter meta_list
              ~f:(fun tlv_meta ->
                  let lifetime = Time.diff now tlv_meta.stamp in
                  Time.Span.(<) lifetime tlv_meta.ttl)
          in
          Some meta_list)
;;

let clean t =
  Throttle.enqueue t.sequencer (fun () -> clean_aux t; Deferred.unit)
;;

let add t ~interface lldp =
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
               tlv_stash :=
                 Map.change !tlv_stash interface
                   ~f:(fun meta_list ->
                       let new_meta = { tlv = new_tlv; ttl; stamp = Time.now () } in
                       match meta_list with
                       | None           -> Some [ new_meta ]
                       | Some meta_list ->
                         (* Before adding a new record remove duplicates from the stash *)
                         let meta_list =
                           List.filter meta_list
                             ~f:(fun meta -> Lldp.Tlv.compare new_tlv meta.tlv <> 0)
                         in
                         Some (new_meta::meta_list)));
       (* Write the database *)
       write_db t ~interface)
;;

let get _t ~interface = 
  Map.find !tlv_stash interface
  |> Option.map ~f:(List.map ~f:(fun meta -> meta.tlv))
;;
