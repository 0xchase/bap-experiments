open Core_kernel
open Bap_main
open Bap.Std
open Format

let counter = object
        inherit [int * int] Term.visitor
        method! enter_term _ _ (jmps,total) = jmps,total+1
        method! enter_jmp _ (jmps,total) = jmps+1, total
end

(* Print ratio of jmps to instructions *)
let ratio proj = 
        let jmps, total = counter#run (Project.program proj) (0, 0) in
        printf "ratio = %d/%d = %g\n" jmps total (float jmps /. float total)

(* Print architecture information *)
let info proj = 
      printf "Running plugin\n";
      let a =
        match Project.arch proj with
        | #Arch.arm -> "ARM"
        | #Arch.x86 -> "x86"
        | #Arch.mips -> "MIPS"
        | _ -> "Unknown"
        in printf "Detected architecture: %s\n" a

let print_insns proj =
        Seq.iter (Disasm.insns (Project.disasm proj)) ~f:(fun (mem,insn) ->
                Printf.printf "%s\n"
                        (Insn.asm insn))

let print_subs proj =
        (Disasm.insns (Project.disasm proj))

let zero_collector = object
        inherit [Tid.Set.t] Term.visitor
        method! enter_def t zeros = match Def.rhs t with
        | Bil.Int x when Word.is_zero x -> Set.add zeros (Term.tid t)
        | _ -> zeros
end

let test proj =
        print_endline "Doing some stuff";
        zero_collector#run (Project.program proj) Tid.Set.empty |> 
        Set.iter ~f:(Format.printf "%a@\n" Tid.pp)
        
(* Print sections *)
let print_sections proj =
        Project.memory proj |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
                Option.iter (Value.get Image.section x) ~f:(fun name ->
                        printf "Section: %s\n" name))

let () = Extension.declare @@ fun _ctxt ->
        Project.register_pass' test;
        Ok ()
