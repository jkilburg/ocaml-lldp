open Core

val siocgifname    : int -> int -> (string,int) Result.t
val siocgifindex   : int -> string -> (int,int) Result.t
val siocgifflags   : int -> string -> (int,int) Result.t
val siocgifpflags  : int -> string -> (int,int) Result.t
val siocgifmtu     : int -> string -> (int,int) Result.t
val siocgifaddr    : int -> string -> (int32,int) Result.t
val siocgifbrdaddr : int -> string -> (int32,int) Result.t
val siocgifnetmask : int -> string -> (int32,int) Result.t
val siocgifhwaddr  : int -> string -> (string,int) Result.t
  
val siocgifconf : int -> ((string * int32) list, int) Result.t
  
val siocsifflags  : int -> string -> int -> (unit,int) Result.t
val siocsifpflags : int -> string -> int -> (unit,int) Result.t
