(*************************************************************************)
(*                                                                       *)
(*                              OCamlClean                               *)
(*                                                                       *)
(*            Benoit Vaugon, Université Pierre et Marie Curie            *)
(*                                                                       *)
(*    Ce fichier est distribué sous les termes de la licence CeCILL-B    *)
(*    décrite dans le fichier ../LICENCE.                                *)
(*                                                                       *)
(*************************************************************************)

open Instr

let compute_map orig_code =
  let nb_instr = Array.length orig_code in
  let map = Array.make nb_instr 0 in
  let rec f i j =
    if i < nb_instr then (
      map.(i) <- j;
      match orig_code.(i) with
	| Nop | Pop 0 -> f (succ i) j
	| _ -> f (succ i) (succ j)
    )
  in
    f 0 0;
    map
;;

let compress orig_code map =
  let orig_size = Array.length orig_code in
  let new_size = map.(Array.length map - 1) + 1 in
  let new_code = Array.make new_size orig_code.(0) in
    for i = 0 to orig_size - 1 do
      new_code.(map.(i)) <- orig_code.(i);
    done;
    new_code
;;

let remap new_code map =
  let nb_instr = Array.length new_code in
  let remap_ptr ptr = ptr.instr_ind <- map.(ptr.instr_ind) in
    for i = 0 to nb_instr - 1 do
      match new_code.(i) with
	| Pushretaddr ptr | Closure (_, ptr) | Branch ptr | Branchif ptr
	| Branchifnot ptr | Pushtrap ptr | Beq (_,ptr) | Bneq (_,ptr)
	| Blint (_,ptr) | Bleint (_,ptr) | Bgtint (_,ptr) | Bgeint (_,ptr)
	| Bultint (_,ptr) | Bugeint (_,ptr) ->
	    remap_ptr ptr;
	| Switch (_, tab) -> Array.iter remap_ptr tab;
	| Closurerec (_, _, ptr, tab) -> remap_ptr ptr;Array.iter remap_ptr tab;
	| _ -> ()
    done;
;;

let clean orig_code =
  let map = compute_map orig_code in
  let new_code = compress orig_code map in
    remap new_code map;
    new_code
;;
