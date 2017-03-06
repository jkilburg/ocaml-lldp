open Core

external so_priority : int -> int -> (unit,int) Result.t = "socket_so_priority"
