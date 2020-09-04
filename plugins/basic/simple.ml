open Core_kernel
open Bap_main
open Bap.Std

let main proj =
  print_endline "My analysis is running!";
  proj

let () = Extension.declare @@ fun _ctxt ->
   Project.register_pass main

