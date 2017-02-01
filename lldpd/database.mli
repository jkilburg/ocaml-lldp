open! Core.Std
open Async.Std

module Destination : sig
  type t = Stdout
         | Nowhere
         | Collector of Unix.Inet_addr.t * int
         | Filename of string

  val expressions : string list

  val of_string : string -> t
end

type t

val create
  :  destination:Destination.t
  -> incoming_ttl:Time.Span.t
  -> unit
  -> t Deferred.t

val add    : t -> interface:string -> Lldp.t -> unit Deferred.t
val clean  : t -> unit Deferred.t
val get    : t -> interface:string -> Lldp.Tlv.t list option
