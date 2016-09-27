open Core.Std

let () =
  Lldp.test () |> Sexp.to_string |> printf "%s\n"
;;
