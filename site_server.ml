open! Core.Std
open Async.Std

let server_map = ref Unix.Inet_addr.Map.empty

let send_packet ~fd ~interface ~tlvs iobuf =
  Iobuf.rewind iobuf;
  Iobuf.Fill.uint8 iobuf (List.length tlvs);
  List.iter tlvs
    ~f:(fun tlv -> Iobuf.Fill.bin_prot Lldp.Tlv.bin_writer_t iobuf tlv);
  Iobuf.rewind iobuf;
  Iobuf.write iobuf (Unix.Fd.file_descr_exn fd);
  Deferred.unit
;;

let parse_packet iobuf addr =
  let `Inet (addr,_port) = addr in
  let rec reader accum = function
    | 0     -> accum
    | count ->
      reader ((Iobuf.Consume.bin_prot Lldp.Tlv.bin_reader_t iobuf)::accum) (count - 1)
  in
  match reader [] (Iobuf.Consume.uint8 iobuf) with
  | []   -> printf !"Read zero tlvs from %{Unix.Inet_addr}\n" addr
  | tlvs -> server_map := Map.change !server_map addr ~f:(fun _ -> Some tlvs)
;;

let tlv_download_rpc () =
  Rpc.Pipe_rpc.create ~name:"tlv-download" ~version:0
    ~bin_query:Unix.Inet_addr.bin_t
    ~bin_response:Lldp.Tlv.bin_t
    ~bin_error:Error.bin_t
    ()
;;
    
let setup_rpc_server ~rpc_port () =
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Pipe_rpc.implement_direct (tlv_download_rpc ())
            (fun _cstate addr writer ->
               match Map.find !server_map addr with
               | None      ->
                 Deferred.Or_error.errorf !"No TLVs found for %{Unix.Inet_addr}" addr
               | Some tlvs ->
                 List.iter tlvs
                   ~f:(fun tlv -> Rpc.Pipe_rpc.Direct_stream_writer.write writer tlv |> ignore);
                 Rpc.Pipe_rpc.Direct_stream_writer.close writer;
                 Deferred.Or_error.ok_unit)
        ]
      ~on_unknown_rpc:`Raise
  in
  Rpc.Connection.serve
    ~implementations
    ~initial_connection_state:(fun _ _ -> ())
    ~where_to_listen:(Tcp.Where_to_listen.create
                        ~socket_type:Socket.Type.tcp
                        ~address:(Socket.Address.Inet.create_bind_any ~port:rpc_port)
                        ~listening_on:(fun (`Inet (_,port)) -> port))
    ()                  
;;

let site_server_main ~port ~rpc_port () =
  every (Time.Span.of_sec 10.)
    (fun () ->
       Map.iteri !server_map
         ~f:(fun ~key ~data ->
             printf !"%{Unix.Inet_addr}:\n" key;
             printf !"%{sexp:Lldp.Tlv.t list}\n" data));
  setup_rpc_server ~rpc_port ()
  >>= fun _server ->
  let socket = Unix.Socket.create Unix.Socket.Type.udp in
  Unix.Socket.bind socket (Socket.Address.Inet.create_bind_any ~port)
  >>= fun socket ->
  Udp.recvfrom_loop (Unix.Socket.fd socket) parse_packet
  >>= fun () ->
  Deferred.Or_error.ok_unit
;;

let server_command () =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Collect TLVs for a site"
    [%map_open
      let port =
        flag "-port" (optional_with_default 9898 int)
          ~doc:"PORT port on which to listen (default:9898)"          
      and rpc_port =
        flag "-rpc-port" (optional_with_default 9897 int)
          ~doc:"PORT port on which to listen for RPC connections (default:9897)"
      in
      site_server_main ~port ~rpc_port
    ]
;;

let query_site_server_main ~host ~port ~query () =
  Rpc.Connection.with_client ~host ~port
    (fun rpc_connection ->
       Rpc.Pipe_rpc.dispatch_exn
         (tlv_download_rpc ()) rpc_connection (Unix.Inet_addr.of_string query)         
       >>= fun (pipe, _id) ->
       Pipe.iter pipe
         ~f:(fun tlv -> printf !"%{sexp:Lldp.Tlv.t}\n" tlv; Deferred.unit))
  >>| Or_error.of_exn_result
;;

let query_command () =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Query the TLVs of a site server"
    [%map_open
      let port =
        flag "-port" (optional_with_default 9897 int)
          ~doc:"PORT port on which to contact site server (default:9897)"
      and host =
        flag "-host" (required string)
          ~doc:"HOSTNAME-OR-IP hostname or IP to contact"
      and query =
        flag "-query" (required string)
          ~doc:"HOSTNAME-OR-IP hostname or IP to query"
      in
      query_site_server_main ~host ~port ~query
    ]
;;
