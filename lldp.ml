open Core.Std

module Inet_addr = Unix.Inet_addr
                     
module C = Iobuf.Consume
module F = Iobuf.Fill

let lldp_protocol_number = 0x88cc

module Mac_address : sig
  type t [@@deriving sexp]

  val of_string : string -> t
  val to_string : t -> string

  val of_byte_list : int list -> t

  val nearest_bridge          : unit -> t
  val nearest_nontpmr_bridge  : unit -> t
  val nearest_customer_bridge : unit -> t
end = struct
  type t = string [@@deriving sexp]

  let of_string = Fn.id
  let to_string = Fn.id
                                
  let of_byte_list l =
    String.of_char_list (List.map l ~f:Char.of_int_exn)
  ;;

  let nearest_bridge () =
    of_byte_list [ 0x01; 0x80; 0xc2; 0x00; 0x00; 0x0e ]
  ;;

  let nearest_nontpmr_bridge () =
    of_byte_list [ 0x01; 0x80; 0xc2; 0x00; 0x00; 0x03 ]
  ;;

  let nearest_customer_bridge () =
    of_byte_list [ 0x01; 0x80; 0xc2; 0x00; 0x00; 0x00 ]
  ;;
end

module Tlv = struct
  let type_and_len ~tlv_type ~tlv_len =
    (tlv_type land 0xef) * 512 + tlv_len
  ;;

  let string_iobuf ~tlv_type x =
    let tlv_len = String.length x in
    let buf = Iobuf.create ~len:(2 + tlv_len) in
    F.uint16_be buf (type_and_len ~tlv_type ~tlv_len);
    F.string buf x;
    buf
  ;;

  module System_capabilities_data = struct
    let tlv_type = 7

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
      [@@deriving sexp]

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
        | x  -> failwithf "Undefined system capability bit: %d" x ()
      ;;

      let to_int = function
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
    
    type t = System_capability_bit.t list [@@deriving sexp]

    let of_iobuf buf =
      let mask = C.uint16_be buf in
      let rec check_bit ~t = function
        | 16  -> List.rev t
        | bit ->
          let t =
            if Int.shift_right mask bit land 0x0001 = 1
            then (System_capability_bit.of_int bit)::t
            else t
          in
          check_bit ~t (bit + 1)
      in
      check_bit ~t:[] 0
    ;;

    let to_iobuf ~caps ~enabled =
      let rec set_bit ~mask = function
        | []     -> mask
        | hd::tl ->
          let mask =
            mask lor Int.shift_left 1 (System_capability_bit.to_int hd)
          in
          set_bit ~mask tl
      in
      let buf = Iobuf.create ~len:6 in
      F.uint16_be buf (type_and_len ~tlv_type ~tlv_len:4);
      F.uint16_be buf (set_bit ~mask:0 caps);
      F.uint16_be buf (set_bit ~mask:0 enabled);
      buf
    ;;
  end

  module Chassis_id_data = struct
    let tlv_type = 1

    type t = Reserved          of int * string
           | Chassis_component of string
           | Interface_alias   of string
           | Port_component    of string
           | Mac_address       of Mac_address.t
           | Network_address   of Inet_addr.Blocking_sexp.t
           | Interface_name    of string
           | Local             of string
    [@@deriving sexp]

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

    let to_iobuf t =
      let (subtype,data) =
        match t with
        | Chassis_component s  -> (1,s)
        | Interface_alias s    -> (2,s)
        | Port_component s     -> (3,s)
        | Mac_address mac      -> (4,Mac_address.to_string mac)
        | Network_address ip   -> (5,Inet_addr.to_string ip)
        | Interface_name s     -> (6,s)
        | Local s              -> (7,s) 
        | Reserved (subtype,s) -> (subtype,s)
      in
      let tlv_len = String.length data + 1 in
      let buf = Iobuf.create ~len:(tlv_len + 2) in
      F.uint16_be buf (type_and_len ~tlv_type ~tlv_len);
      F.uint8 buf subtype;
      F.string buf data;
      buf
    ;;
  end

  module Port_id_data = struct
    let tlv_type = 2

    type t = Reserved         of int * string
           | Interface_alias  of string
           | Port_component   of string
           | Mac_address      of Mac_address.t
           | Network_address  of Inet_addr.Blocking_sexp.t
           | Interface_name   of string
           | Agent_circuit_id of string
           | Local            of string
    [@@deriving sexp]

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

    let to_iobuf t =
      let (subtype,data) =
        match t with
        | Interface_alias s    -> (1,s)
        | Port_component s     -> (2,s)
        | Mac_address mac      -> (3,Mac_address.to_string mac)
        | Network_address ip   -> (4,Inet_addr.to_string ip)
        | Interface_name s     -> (5,s)
        | Agent_circuit_id s   -> (6,s)
        | Local s              -> (7,s)
        | Reserved (subtype,s) -> (subtype,s)
      in
      let tlv_len = String.length data + 1 in
      let buf = Iobuf.create ~len:(tlv_len + 2) in
      F.uint16_be buf (type_and_len ~tlv_type ~tlv_len);
      F.uint8 buf subtype;
      F.string buf data;
      buf
    ;;
  end

  module Organizational_data = struct
    let tlv_type = 127

    module Ieee_802_1 = struct
      type t = Reserved of int * string
             | Port_vlan_id of int
             | Port_and_protocol_vlan_id of int * int
             | Vlan_name of int * string
             | Protocol_identity of string
      [@@deriving sexp]

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

      let to_iobuf _t _buf = ()
    end

    module Ieee_802_3 = struct
      type phy_configuration_status =
        { autoneg_supported  : bool
        ; autoneg_enabled    : bool
        ; autoneg_capability : int
        ; mau_type           : int
        }
      [@@deriving sexp]

      type link_aggregation_status =
        { supported : bool
        ; enabled   : bool
        ; port_id   : int
        }
      [@@deriving sexp]

      type power_status =
        { port_class         : int
        ; pse_supported      : bool
        ; pse_enabled        : bool
        ; pairs_controllable : bool
        ; power_pair         : int
        ; power_class        : int
        }
      [@@deriving sexp]
        
      type t = Reserved of int * string
             | Phy_configuration_status of phy_configuration_status
             | Power_via_mdi of power_status
             | Link_aggregation of link_aggregation_status
             | Maximum_frame_size of int
      [@@deriving sexp]

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
            ; pse_supported      = power_support land 0x02 = 0x02
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

      let to_iobuf _t _buf = ()
    end

    type t = Unknown of int * int * int * int * string
           | Ieee_802_1 of Ieee_802_1.t
           | Ieee_802_3 of Ieee_802_3.t
    [@@deriving sexp]

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

    let to_iobuf _t =
      failwith "Not implemented"
    ;;
  end

  module Management_address_data = struct
    let tlv_type = 8

    type t =
      { management_address_subtype  : int
      ; management_address          : string
      ; interface_numbering_subtype : int
      ; interface_number            : int
      ; oid                         : string
      }
    [@@deriving sexp]

    let of_iobuf buf =
      let mlen                        = C.uint8 buf in
      let management_address_subtype  = C.uint8 buf in
      let management_address          = C.string ~len:mlen buf in
      let interface_numbering_subtype = C.uint8 buf in
      let interface_number            = C.uint32_be buf in
      let olen                        = C.uint8 buf in
      let oid                         = C.string ~len:olen buf in
      { management_address_subtype
      ; management_address
      ; interface_numbering_subtype
      ; interface_number
      ; oid
      }
    ;;

    let to_iobuf t =
      let mlen = String.length t.management_address in
      let olen = String.length t.oid in
      let tlv_len =
        (* management_address_length + subtype + address *)
        1 + 1 + mlen
        (* interface_numbering_subtype + interface_number (4 bytes) *)
        + 1 + 4
        (* oid length + oid *)
        + 1 + olen
      in
      let buf = Iobuf.create ~len:(2 + tlv_len) in
      F.uint16_be buf (type_and_len ~tlv_type ~tlv_len);
      F.uint8 buf mlen;
      F.string buf t.management_address;
      F.uint8 buf t.interface_numbering_subtype;
      F.uint32_be buf t.interface_number;
      F.uint8 buf olen;
      F.string buf t.oid;
      buf
    ;;
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
  [@@deriving sexp]

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

  let ttl_iobuf x =
    let buf = Iobuf.create ~len:4 in
    F.uint16_be buf (type_and_len ~tlv_type:3 ~tlv_len:2);
    F.uint16_be buf x;
    buf
  ;;

  let last_tlv_iobuf () =
    (* tlv_type = 0 *)
    let buf = Iobuf.create ~len:2 in
    F.uint16_be buf 0;
    buf
  ;;

  let to_iobuf = function
    | Last_tlv              -> last_tlv_iobuf ()           (* tlv_type = 0 *)
    | Chassis_id x          -> Chassis_id_data.to_iobuf x  (* 1 *)
    | Port_id x             -> Port_id_data.to_iobuf x     (* 2 *)
    | Ttl x                 -> ttl_iobuf x                 (* 3 *)
    | Port_description x    -> string_iobuf ~tlv_type:4 x
    | System_name x         -> string_iobuf ~tlv_type:5 x
    | System_description x  -> string_iobuf ~tlv_type:6 x
    | System_capabilities (caps,enabled) ->
      System_capabilities_data.to_iobuf ~caps ~enabled     (* 7 *)
    | Management_address x  -> Management_address_data.to_iobuf x (* 8 *)
    | Organizational x      -> Organizational_data.to_iobuf x (* 127 *)
    | Reserved (tlv_type,x) -> string_iobuf ~tlv_type x
  ;;
end

type t =
  { destination_mac : Mac_address.t
  ; source_mac      : Mac_address.t
  ; tlvs            : Tlv.t list
  } [@@deriving sexp]

let of_iobuf buf =
  let destination_mac = Mac_address.of_string (C.string ~len:6 buf) in
  let source_mac      = Mac_address.of_string (C.string ~len:6 buf) in
  let ether_type      = C.uint16_be buf in
  let rec parse_tlvs tlvs =
    match Tlv.of_iobuf buf with
    | Tlv.Last_tlv -> List.rev tlvs
    | tlv          -> parse_tlvs (tlv::tlvs)
  in
  match ether_type with
  | 0x88cc -> { destination_mac; source_mac; tlvs = parse_tlvs [] }
  | x      -> failwithf "Unknown ether_type: %x" x ()
;;

let to_iobuf_list t =
  let buf = Iobuf.create ~len:14 in
  F.string buf (Mac_address.to_string t.destination_mac);
  F.string buf (Mac_address.to_string t.source_mac);
  F.uint16_be buf lldp_protocol_number;
  let tlv_bufs = List.map t.tlvs ~f:Tlv.to_iobuf in
  let last_buf = Tlv.to_iobuf Tlv.Last_tlv in
  buf :: ( tlv_bufs @ [ last_buf ] )
;;

let to_iobuf t =
  let iobufs = to_iobuf_list t in
  let len =
    List.fold iobufs ~init:0
      ~f:(fun acc iobuf ->
          Iobuf.rewind iobuf;
          printf "%s\n" (Iobuf.to_string_hum ~bounds:`Whole iobuf);
          acc + Iobuf.length iobuf)
  in
  let buf = Iobuf.create ~len in
  List.iter iobufs
    ~f:(fun iobuf -> Iobuf.Blit_fill.blito ~src:iobuf ~dst:buf ());
  Iobuf.rewind buf;
  buf
;;

let test () =
  to_iobuf
    { destination_mac = Mac_address.nearest_bridge ()
    ; source_mac      = Mac_address.of_byte_list [ 1; 2; 3; 4; 5; 6 ]
    ; tlvs            =
        [ Tlv.Chassis_id (Tlv.Chassis_id_data.Local "myhostname")
        ; Tlv.Port_id    (Tlv.Port_id_data.Interface_name "eth0")
        ; Tlv.Ttl        30
        ]
    }
  |> of_iobuf
;;
