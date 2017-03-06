open Core

val socket : int -> (int,int) Result.t
val bind   : int -> int -> int -> (unit,int) Result.t
val add_membership  : int -> int -> string -> (unit,int) Result.t
val drop_membership : int -> int -> string -> (unit,int) Result.t
val origdev         : int -> int -> (unit,int) Result.t
