open Core.Std

module Inet_addr = Unix.Inet_addr
                     
module C = Iobuf.Consume

module Mac_address : sig
  type t with sexp

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string with sexp

  let of_string = Fn.id
  let to_string = Fn.id
end

module Tlv = struct
  module System_capabilities_data = struct
    module System_capability_bit = struct
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
      with sexp

      let of_int = function
        | 0  -> Other
        | 1  -> Repeater
        | 2  -> Bridge
        | 3  -> Wlan_AP
        | 4  -> Router
        | 5  -> Telephone
        | 6  -> DOCSIS_cable_device
        | 7  -> Station_only
        | 8  -> Reserved_8
        | 9  -> Reserved_9
        | 10 -> Reserved_10
        | 11 -> Reserved_11
        | 12 -> Reserved_12
        | 13 -> Reserved_13
        | 14 -> Reserved_14
        | 15 -> Reserved_15
        | x  -> failwithf "Undefined system capabiliy bit: %d" x ()
      ;;

      let to_int t = function
        | Other               -> 0
        | Repeater            -> 1
        | Bridge              -> 2
        | Wlan_AP             -> 3
        | Router              -> 4
        | Telephone           -> 5
        | DOCSIS_cable_device -> 6
        | Station_only        -> 7
        | Reserved_8          -> 8
        | Reserved_9          -> 9
        | Reserved_10         -> 10
        | Reserved_11         -> 11
        | Reserved_12         -> 12
        | Reserved_13         -> 13
        | Reserved_14         -> 14
        | Reserved_15         -> 15
      ;;
    end
    
    type t = System_capability_bit.t list with sexp

    let of_iobuf buf =
      let mask = C.uint16_be buf in
      let rec check_bit ~t ~mask bit =
        if bit = 16
        then List.rev t
        else
          let t =
            if mask land 0x0001 = 1
            then (System_capability_bit.of_int bit)::t
            else t
          in
          check_bit ~t ~mask:(Int.shift_right mask 1) (bit + 1)
      in
      check_bit ~t:[] ~mask 0
    ;;
  end

  module Chassis_id_data = struct
    type t = Reserved          of int * string
           | Chassis_component of string
           | Interface_alias   of string
           | Port_component    of string
           | Mac_address       of Mac_address.t
           | Network_address   of Inet_addr.t
           | Interface_name    of string
           | Local             of string
    with sexp

    let of_iobuf buf tlv_len  =
      let subtype = C.uint8 buf in
      let s       = C.string ~len:(tlv_len - 1) buf in
      match subtype with
      | 1 -> Chassis_component s
      | 2 -> Interface_alias s
      | 3 -> Port_component s
      | 4 -> Mac_address (Mac_address.of_string s)
      | 5 -> Network_address (Inet_addr.of_string s)
      | 6 -> Interface_name s
      | 7 -> Local s
      | x -> Reserved (x,s)
    ;;
  end

  module Port_id_data = struct
    type t = Reserved         of int * string
           | Interface_alias  of string
           | Port_component   of string
           | Mac_address      of Mac_address.t
           | Network_address  of Inet_addr.t
           | Interface_name   of string
           | Agent_circuit_id of string
           | Local            of string
    with sexp

    let of_iobuf buf tlv_len =
      let subtype = C.uint8 buf in
      let s       = C.string ~len:(tlv_len - 1) buf in
      match subtype with
      | 1 -> Interface_alias s
      | 2 -> Port_component s
      | 3 -> Mac_address (Mac_address.of_string s)
      | 4 -> Network_address (Inet_addr.of_string s)
      | 5 -> Interface_name s
      | 6 -> Agent_circuit_id s
      | 7 -> Local s
      | x -> Reserved (x,s)
    ;;
  end

  module Organizational_data = struct
    module Ieee_802_1 = struct
      type t = Reserved of int * string
             | Port_vlan_id of int
             | Port_and_protocol_vlan_id of int * int
             | Vlan_name of int * string
             | Protocol_identity of string
      with sexp

      let of_iobuf buf tlv_len =
        let subtype = C.uint8 buf in
        match subtype with
        | 1 -> Port_vlan_id (C.uint16_be buf)
        | 2 -> Port_and_protocol_vlan_id (C.uint8 buf,C.uint16_be buf)
        | 3 ->
          let vlan_id       = C.uint16_be buf in
          let vlan_name_len = C.uint8 buf in
          let vlan_name     = C.string ~len:vlan_name_len buf in
          Vlan_name (vlan_id, vlan_name)
        | 4 ->
          let protocol_id_len = C.uint8 buf in
          let protocol_id     = C.string ~len:protocol_id_len buf in
          Protocol_identity protocol_id
        | _ -> Reserved (subtype,C.string ~len:(tlv_len - 4) buf)          
      ;;
    end

    module Ieee_802_3 = struct
      type phy_configuration_status =
        { autoneg_supported  : bool
        ; autoneg_enabled    : bool
        ; autoneg_capability : int
        ; mau_type           : int
        }
      with sexp

      type link_aggregation_status =
        { supported : bool
        ; enabled   : bool
        ; port_id   : int
        }
      with sexp

      type power_status =
        { port_class         : int
        ; pse_supported      : bool
        ; pse_enabled        : bool
        ; pairs_controllable : bool
        ; power_pair         : int
        ; power_class        : int
        }
      with sexp
        
      type t = Reserved of int * string
             | Phy_configuration_status of phy_configuration_status
             | Power_via_mdi of power_status
             | Link_aggregation of link_aggregation_status
             | Maximum_frame_size of int
      with sexp

      let of_iobuf buf tlv_len =
        let subtype = C.uint8 buf in
        match subtype with
        | 1 ->
          let autoneg_status     = C.uint8 buf in
          let autoneg_capability = C.uint16_be buf in
          let mau_type           = C.uint16_be buf in
          Phy_configuration_status
            { autoneg_supported = autoneg_status land 0x01 = 1
            ; autoneg_enabled   = autoneg_status land 0x02 = 2
            ; autoneg_capability
            ; mau_type
            }
        | 2 ->
          let power_support  = C.uint8 buf in
          let power_pair     = C.uint8 buf in
          let power_class    = C.uint8 buf in
          Power_via_mdi
            { port_class         = power_support land 0x01
            ; pse_supported      = power_support land 0x02 = 0x04
            ; pse_enabled        = power_support land 0x04 = 0x04
            ; pairs_controllable = power_support land 0x08 = 0x08
            ; power_pair
            ; power_class
            }
        | 3 ->
          let aggregation_status = C.uint8 buf in
          let port_id            = C.uint32_be buf in
          Link_aggregation
            { supported = aggregation_status land 0x01 = 1
            ; enabled   = aggregation_status land 0x02 = 2
            ; port_id
            }
        | 4 ->
          let frame_size = C.uint16_be buf in
          Maximum_frame_size frame_size
        | _ -> Reserved (subtype,C.string ~len:(tlv_len - 4) buf)          
      ;;
    end
    
    type t = Unknown of int * int * int * int * string
           | Ieee_802_1 of Ieee_802_1.t
           | Ieee_802_3 of Ieee_802_3.t
    with sexp
      
    let of_iobuf buf tlv_len =
      let b1 = C.uint8 buf in
      let b2 = C.uint8 buf in
      let b3 = C.uint8 buf in
      match (b1,b2,b3) with
      | (0x00,0x80,0xc2) -> Ieee_802_1 (Ieee_802_1.of_iobuf buf tlv_len)
      | (0x00,0x12,0x0f) -> Ieee_802_3 (Ieee_802_3.of_iobuf buf tlv_len)
      | _                ->
        let subtype = C.uint8 buf in
        let data = C.string ~len:(tlv_len - 4) buf in
        Unknown (b1,b2,b3,subtype,data)
    ;;
  end

  module Management_address_data = struct
    type t =
      { management_address_subtype  : int
      ; management_address          : string
      ; interface_numbering_subtype : int
      ; interface_number            : int
      ; oid                         : string
      }
    with sexp

    let of_iobuf buf =
      let slen                        = C.uint8 buf in
      let management_address_subtype  = C.uint8 buf in
      let management_address          = C.string ~len:slen buf in
      let interface_numbering_subtype = C.uint8 buf in
      let interface_number            = C.uint32_be buf in
      let oid_slen                    = C.uint8 buf in
      let oid                         = C.string ~len:oid_slen buf in
      { management_address_subtype
      ; management_address
      ; interface_numbering_subtype
      ; interface_number
      ; oid
      }
    ;;
  end

  type t = Last_tlv
         | Chassis_id          of Chassis_id_data.t
         | Port_id             of Port_id_data.t
         | Ttl                 of int
         | Port_description    of string
         | System_name         of string
         | System_capabilities of System_capabilities_data.t * System_capabilities_data.t
         | System_description  of string
         | Management_address  of Management_address_data.t
         | Reserved            of int * string
         | Organizational      of Organizational_data.t
  with sexp
    
  let of_iobuf buf =
    let b1 = C.uint8 buf in
    let b2 = C.uint8 buf in
    let tlv_type = Int.shift_right b1 1 in
    let tlv_len  = b2 + (b1 land 0x01) * 256 in
    match tlv_type with
    | 0   -> Last_tlv
    | 1   -> Chassis_id (Chassis_id_data.of_iobuf buf tlv_len)
    | 2   -> Port_id (Port_id_data.of_iobuf buf tlv_len)
    | 3   -> Ttl (C.uint16_be buf)
    | 4   -> Port_description (C.string ~len:tlv_len buf)
    | 5   -> System_name (C.string ~len:tlv_len buf)
    | 6   -> System_description (C.string ~len:tlv_len buf)
    | 7   ->
      let caps    = System_capabilities_data.of_iobuf buf in
      let enabled = System_capabilities_data.of_iobuf buf in
      System_capabilities (caps, enabled)
    | 8   -> Management_address (Management_address_data.of_iobuf buf)
    | 127 -> Organizational (Organizational_data.of_iobuf buf tlv_len)
    | x   -> Reserved (x, C.string ~len:tlv_len buf)
  ;;
end

type t =
  { destination_mac : Mac_address.t
  ; source_mac      : Mac_address.t
  ; tlvs            : Tlv.t list
  } with sexp

let of_iobuf buf =
  let destination_mac = Mac_address.of_string (C.string ~len:6 buf) in
  let source_mac      = Mac_address.of_string (C.string ~len:6 buf) in
  let ether_type      = C.uint16_be buf in
  let rec read_tlvs tlvs =
    match Tlv.of_iobuf buf with
    | Tlv.Last_tlv -> List.rev tlvs
    | tlv          -> read_tlvs (tlv::tlvs)
  in
  match ether_type with
  | 0x88cc -> sexp_of_t { destination_mac; source_mac; tlvs = read_tlvs [] }
  | x      -> failwithf "Unknown ether_type: %x" x ()
;;
