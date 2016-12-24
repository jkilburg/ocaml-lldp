open! Core.Std
open Async.Std

val send_packet
  :  fd:Unix.Fd.t
  -> interface:string
  -> tlvs:Lldp.Tlv.t list 
  -> (read_write, Iobuf.seek) Iobuf.t 
  -> unit Deferred.t

val server_command : unit -> Command.t

val query_command  : unit -> Command.t
