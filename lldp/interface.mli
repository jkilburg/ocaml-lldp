open! Core
open Async

type t

val create
  :  outgoing_ttl:Time.Span.t
  -> transmit_interval:Time.Span.t
  -> string
  -> t Deferred.Or_error.t

val create_read_only : string -> t Deferred.Or_error.t

val fd : t -> Unix.Fd.t

val close : t -> unit Deferred.Or_error.t

val read : t -> Lldp.t Deferred.Or_error.t

val name : t -> string
  
