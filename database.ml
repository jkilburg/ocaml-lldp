open! Core.Std
open Async.Std

type t =
  { sequencer    : unit Throttle.Sequencer.t
  ; incoming_ttl : Time.Span.t
  ; db_filename  : string option
  }

let create ?db_filename ~incoming_ttl () =
  let sequencer = Throttle.Sequencer.create () in
  { sequencer; incoming_ttl; db_filename }
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
  match t.db_filename with
  | None          -> printf !"%{sexp:meta list}\n" !tlv_stash; Deferred.unit
  | Some filename -> Writer.save_sexp filename ([%sexp_of: meta list] !tlv_stash)
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

let get _stash = List.map !tlv_stash ~f:(fun meta -> meta.tlv) ;;
