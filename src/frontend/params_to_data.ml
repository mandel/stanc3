open Core_kernel
open Ast

let rec powerset s =
  match s with
  | [] -> [[]]
  | x :: xs ->
      let ps = powerset xs in
      ps @ List.map ~f:(fun s -> x :: s) ps


let move_params p =
  let eq a b = compare_untyped_statement a b = 0 in
  match p.parametersblock with
  | None -> [ p ]
  | Some params ->
      let data = Option.value ~default:[] p.datablock in
      List.map
        ~f:(fun s ->
          { p with
            datablock = Some (data @ s);
            parametersblock =
            Some (List.filter ~f:(fun x -> not (List.mem ~equal:eq s x)) params); })
        (powerset params)

let gen_programs ast =
  List.iter
    ~f:(fun p ->
      Format.printf "/*---------*/@.";
      Format.printf "%s@." (Pretty_printing.pretty_print_program p))
    (move_params ast);
  exit 0
