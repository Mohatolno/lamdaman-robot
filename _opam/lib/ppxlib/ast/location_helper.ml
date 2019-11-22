
let print_error ppf loc = Format.fprintf ppf "%aError:" Ocaml_common.Location.print_loc loc
let error_of_printer ~loc x y = Ocaml_common.Location.error_of_printer ~loc x y

