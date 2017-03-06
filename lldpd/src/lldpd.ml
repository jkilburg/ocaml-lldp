open! Core
open Async
open Lldp

let local_server_main
    ~destination ~outgoing_ttl ~incoming_ttl ~clean_interval ~interfaces ~receive_only
    ~transmit_interval () =
  Deferred.Or_error.List.map interfaces
    ~f:(fun interface ->
        match receive_only with
        | true  -> Interface.create_read_only interface
        | false -> Interface.create ~outgoing_ttl ~transmit_interval interface)
  >>=? fun all_interfaces ->
  let%bind db = Database.create ~destination ~incoming_ttl () in
  (* Clean the database of incoming LLDP packets every clean_interval *)
  every clean_interval (fun () -> Database.clean db >>> fun () -> ());
  let rec loop closed_or_bad_interfaces =
    let pending_interfaces =
      (* Filter interfaces that caused `Bad_fd or `Closed *)
      List.filter_map all_interfaces
        ~f:(fun interface ->
            match
              List.mem
                ~equal:(fun i1 i2 -> String.equal (Interface.name i1) (Interface.name i2))
                closed_or_bad_interfaces interface
            with
            | true  -> None
            | false ->
              Some (Deferred.both
                      (return interface)
                      (Unix.Fd.ready_to (Interface.fd interface) `Read)))
    in
    match pending_interfaces with
    | []                 ->
      printf "All interfaces closed or bad. Dropping out of read loop.\n";
      return ()
    | pending_interfaces ->
      Deferred.any pending_interfaces
      >>= fun (interface,result) ->
      match result with
      | `Bad_fd
      | `Closed ->
        printf "Dropping interface %s\n" (Interface.name interface);
        loop (interface::closed_or_bad_interfaces)
      | `Ready  ->
        Interface.read interface
        >>= function
        | Ok lldp ->
          Database.add ~interface:(Interface.name interface) db lldp
          >>= fun () ->
          loop closed_or_bad_interfaces
        | Error e ->
          printf "Error while reading: %s\n" (Error.to_string_hum e);
          loop closed_or_bad_interfaces
  in
  loop [] >>| Or_error.return
;;

let local_server () =
  let destination_arg = Command.Arg_type.create Database.Destination.of_string in
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Read and write LLDP packets"
    [%map_open
      let outgoing_ttl =
        flag "-outgoing-ttl"
          (optional_with_default (Time.Span.of_sec 300.0) time_span)
          ~doc:"TIME-SPAN time-to-live for use in transmitted LLDP packets (default: 300s)"
      and incoming_ttl =
        flag "-incoming-ttl"
          (optional_with_default (Time.Span.of_sec 300.0) time_span)
          ~doc:"TIME-SPAN time-to-live to apply to incoming packets \
                without a TTL TLV (default: 300s)"
      and clean_interval =
        flag "-clean-interval"
          (optional_with_default (Time.Span.of_sec 30.0) time_span)
          ~doc:"TIME-SPAN cleanup LLDP database at this interval (default: 30s)"
      and destination =
        flag "-destination" (required destination_arg)
          ~doc:(sprintf "DESTINATION must be one of %s"
                  (String.concat ~sep:"," Database.Destination.expressions))
      and receive_only =
        flag "-receive-only" no_arg ~doc:" do not transmit LLDP. receive only."
      and transmit_interval =
        flag "-transmit-interval"
          (optional_with_default (Time.Span.of_sec 150.0) time_span)
          ~doc:"TIME-SPAN LLDP transmit interval. (default: 150s)"
      and
        interfaces = anon (sequence ( "ETHERNET-INTERFACE-NAME" %: string ))
      in
      fun () ->
        match interfaces with
        | []         ->
          Deferred.Or_error.errorf "Expected at least one ethernet interface name"
        | interfaces ->
          local_server_main
            ~destination
            ~outgoing_ttl ~incoming_ttl ~clean_interval ~interfaces ~receive_only
            ~transmit_interval ()
    ]
;;

let () =
  Command.group ~summary:"LLDP daemon"
    [ "local-server"     , local_server ()
    ; "run-site-server"  , Site_server.server_command ()
    ; "query-site-server", Site_server.query_command ()
    ]
  |> Command.run
;;
