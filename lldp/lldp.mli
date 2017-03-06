open Core

module Inet_addr = Unix.Inet_addr

module Mac_address : sig
  type t [@@deriving sexp, bin_io]

  val of_string : string -> t
  val to_string : t -> string

  val of_byte_list : int list -> t

  val nearest_bridge          : unit -> t
  val nearest_nontpmr_bridge  : unit -> t
  val nearest_customer_bridge : unit -> t
end

module Tlv : sig
  module Chassis_id_data : sig
    type t = Reserved          of int * string
           | Chassis_component of string
           | Interface_alias   of string
           | Port_component    of string
           | Mac_address       of Mac_address.t
           | Network_address   of Inet_addr.Blocking_sexp.t
           | Interface_name    of string
           | Local             of string
    [@@deriving sexp, bin_io]
  end

  module Port_id_data : sig
    type t = Reserved         of int * string
           | Interface_alias  of string
           | Port_component   of string
           | Mac_address      of Mac_address.t
           | Network_address  of Inet_addr.Blocking_sexp.t
           | Interface_name   of string
           | Agent_circuit_id of string
           | Local            of string
    [@@deriving sexp, bin_io]
  end

  module System_capabilities_data : sig
    module System_capability_bit : sig
      type t = Other
             | Repeater
             | Bridge
             | Wlan_AP
             | Router
             | Telephone
             | DOCSIS_cable_device
             | Station_only
             | Reserved_8
             | Reserved_9
             | Reserved_10
             | Reserved_11
             | Reserved_12
             | Reserved_13
             | Reserved_14
             | Reserved_15
      [@@deriving sexp, bin_io]
    end

    type t = System_capability_bit.t list [@@deriving sexp, bin_io]
  end

  module Management_address_data : sig
    type t =
      { management_address_subtype  : int
      ; management_address          : string
      ; interface_numbering_subtype : int
      ; interface_number            : int
      ; oid                         : string
      }
    [@@deriving sexp, bin_io]
  end

  module Organizational_data : sig
    module Ieee_802_1 : sig
      type t = Reserved of int * string
             | Port_vlan_id of int
             | Port_and_protocol_vlan_id of int * int
             | Vlan_name of int * string
             | Protocol_identity of string
      [@@deriving sexp, bin_io]
    end
    module Ieee_802_3 : sig
      type phy_configuration_status =
        { autoneg_supported  : bool
        ; autoneg_enabled    : bool
        ; autoneg_capability : int
        ; mau_type           : int
        }
      [@@deriving sexp, bin_io]

      type link_aggregation_status =
        { supported : bool
        ; enabled   : bool
        ; port_id   : int
        }
      [@@deriving sexp, bin_io]

      type power_status =
        { port_class         : int
        ; pse_supported      : bool
        ; pse_enabled        : bool
        ; pairs_controllable : bool
        ; power_pair         : int
        ; power_class        : int
        }
      [@@deriving sexp, bin_io]
        
      type t = Reserved of int * string
             | Phy_configuration_status of phy_configuration_status
             | Power_via_mdi of power_status
             | Link_aggregation of link_aggregation_status
             | Maximum_frame_size of int
      [@@deriving sexp, bin_io]
    end

    type t = Unknown of int * int * int * int * string
           | Ieee_802_1 of Ieee_802_1.t
           | Ieee_802_3 of Ieee_802_3.t
    [@@deriving sexp, bin_io]
  end

  type t = Last_tlv
         | Chassis_id          of Chassis_id_data.t
         | Port_id             of Port_id_data.t
         | Ttl                 of int
         | Port_description    of string
         | System_name         of string
         | System_capabilities of
             System_capabilities_data.t * System_capabilities_data.t
         | System_description  of string
         | Management_address  of Management_address_data.t
         | Reserved            of int * string
         | Organizational      of Organizational_data.t
  [@@deriving sexp, bin_io, compare]
end

type t =
  { destination_mac : Mac_address.t
  ; source_mac      : Mac_address.t
  ; tlvs            : Tlv.t list
  } [@@deriving sexp, bin_io, fields]

val protocol_number : int

val of_iobuf : ([>Core.read], Iobuf.seek) Iobuf.t -> t
val to_iobuf : t -> ([>Core.write], Iobuf.seek) Iobuf.t

val test : unit -> t
