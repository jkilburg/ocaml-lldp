open! Core.Std
open Async.Std

type t

val create : ?db_filename:string -> incoming_ttl:Time.Span.t -> unit -> t
val add    : t -> Lldp.t -> unit Deferred.t
val clean  : t -> unit Deferred.t
val get    : t -> Lldp.Tlv.t list
