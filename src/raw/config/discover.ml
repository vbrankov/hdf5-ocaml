module C = Configurator.V1

let hdf5_default =
  let libs =
    let libs = ["-lhdf5"; "-lhdf5_hl"] in
    let s = "/usr/lib/x86_64-linux-gnu/hdf5/serial" in
    if Sys.file_exists s then libs @ [Printf.sprintf "-L%s" s] else libs
  in
  let cflags =
    let s = "/usr/include/hdf5/serial" in
    if Sys.file_exists s then [Printf.sprintf "-I%s" s] else []
  in
  C.Pkg_config.{cflags; libs}

let () =
  C.main ~name:"hdf5-raw" (fun c ->
      let conf =
        let open Base.Option.Monad_infix in
        Base.Option.value
          ~default:hdf5_default
          (C.Pkg_config.get c >>= C.Pkg_config.query ~package:"hdf5")
      in
      let conf = {conf with libs = conf.libs @ ["-lhdf5_hl"]} in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs )
