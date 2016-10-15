open! Core.Std
open Async.Std

type t

val create : incoming_ttl:Time.Span.t -> unit -> t
val add    : t -> Lldp.t -> unit Deferred.t
val clean  : t -> unit Deferred.t
val get    : t -> Lldp.t list
