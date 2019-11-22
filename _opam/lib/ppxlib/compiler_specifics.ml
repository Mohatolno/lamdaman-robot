module O = Ocaml_common
let get_load_path () = Ocaml_common.Load_path.get_paths ()
let read_clflags_from_env () = Ocaml_common.Compmisc.read_clflags_from_env ()
