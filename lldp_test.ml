open Core.Std

let () =
  Lldp.test () |> Lldp.sexp_of_t |> Sexp.to_string |> printf "%s\n"
;;
