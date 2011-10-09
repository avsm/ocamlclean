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

let export filename code prim data begn dumps =
  let oc =
    open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] 0o751 filename
  in
    Dump.export oc begn;
    let code_offset = pos_out oc in
    Code.export oc code;
    let prim_offset = pos_out oc in
    let code_length = prim_offset - code_offset in
    Prim.export oc prim;
    let data_offset = pos_out oc in
    let prim_length = data_offset - prim_offset in
    Data.export oc data;
    let end_offset = pos_out oc in
    let data_length = end_offset - data_offset in
    let rec export_dumps dumps_rest dumps_index =
      match dumps_rest with
	| [] -> List.rev dumps_index
	| (section, mem) :: tl ->
	    let length = Dump.size mem in
	      if length <> 0 then
		let offset = pos_out oc in
		Dump.export oc mem;
		export_dumps tl ((section, offset, length) :: dumps_index)
	      else
		export_dumps tl dumps_index
    in
      Index.export oc (
	(Index.Code, code_offset, code_length) ::
	(Index.Prim, prim_offset, prim_length) ::
	(Index.Data, data_offset, data_length) ::
	(export_dumps dumps [])
      );
      let file_length = pos_out oc in
	close_out oc;
	(code_length, data_length, prim_length, file_length)
;;

let export_c filename code prim data begn dumps =
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" "#ifdef __cplusplus
extern \"C\" {
#endif
#include <caml/mlvalues.h>
CAMLextern void caml_startup_code(
code_t code, asize_t code_size,
char *data, asize_t data_size,
char *section_table, asize_t section_table_size,
char **argv);
static int caml_code[] = {";
  Code.export_c oc code;
  Printf.fprintf oc "\n};\n\n";
  Printf.fprintf oc "static char caml_sections[] ={\n";
  let sections = [ ] in
  close_out oc;
  (0,0,0,0)

