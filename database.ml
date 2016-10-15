open! Core.Std
open Async.Std

type t =
  { sequencer    : unit Throttle.Sequencer.t
  ; incoming_ttl : Time.Span.t
  }

let create ~incoming_ttl () =
  let sequencer = Throttle.Sequencer.create () in
  { sequencer; incoming_ttl }
;;

type meta =
  { lldp  : Lldp.t
  ; stamp : Time.t
  } [@@deriving sexp]

let empty () : meta list = []

let lldp_stash = ref (empty ())

let add t lldp =
  Throttle.enqueue t.sequencer
    (fun () ->
       lldp_stash := { lldp; stamp = Time.now () }::(!lldp_stash);
       Deferred.unit)
;;

let clean t =
  let now = Time.now () in
  Throttle.enqueue t.sequencer
    (fun () ->
       lldp_stash := 
         List.filter !lldp_stash
           ~f:(fun lldp_meta ->
               let lifetime = Time.diff now lldp_meta.stamp in
               let ttl =
                 match
                   List.find_map (Lldp.tlvs lldp_meta.lldp)
                     ~f:(function
                         | Lldp.Tlv.Ttl ttl -> Some ttl
                         | _                -> None)
                 with
                 | None     -> t.incoming_ttl
                 | Some ttl -> Time.Span.of_sec (Float.of_int ttl)
               in
               Time.Span.(<) lifetime ttl);
       Deferred.unit)
;;

let get _stash = List.map !lldp_stash ~f:(fun meta -> meta.lldp) ;;
