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

let source_file = ref None in
let dest_file = ref "a.out" in
let verbose = ref false in
let spec =
  Arg.align [
    ("-o", Arg.Set_string dest_file,
     Printf.sprintf "<outfile> define output filename (default: %s)"
       !dest_file);
    ("-verbose", Arg.Set verbose, " Verbose mode");
    ("-version", Arg.Unit (fun () -> print_endline Config.version ; exit 0),
     " Print version and exit");
  ] in
let usage = Printf.sprintf "Usage: %s [ OPTIONS ] <file>.byte" Sys.argv.(0) in
let error msg =
  Printf.printf "Error: %s\n" msg;
  Arg.usage spec usage;
  exit 1;
in
let unknow arg =
  if !source_file = None then
    source_file := Some arg
  else
    error (Printf.sprintf "don't know what to do with: `%s'" arg);
in
  Arg.parse spec unknow usage;
  let print_msg =
    if !verbose then Printf.printf "%s%!" else fun _ -> ()
  in
  let print_done () = print_msg "done\n" in
  let source_file =
    match !source_file with
      | None -> error "bytecode executable source file undefined";
      | Some f -> f
  in
  let compile loadfn exportfn =
    try
      let pass_counter = ref 0 in
      let rec compress_loop orig_code prim data =
	incr pass_counter;
	print_msg (Printf.sprintf "Pass %d... " !pass_counter);
	let code = Step1.clean orig_code data in
	let  ()  = Step2.clean code prim in
	let  ()  = Step3.clean code in
	let data = Data.clean code data in
	let  ()  = Cleanbra.clean code in
	let code = Rmnop.clean code in
	  print_done ();
	  if orig_code = code then
	    (code, data)
	  else
	    compress_loop code prim data
      in
      print_msg (Printf.sprintf "Loading `%s'... " source_file);
      let (code, prim, data, begn, dumps, orig_code_length,
	   orig_data_length, orig_prim_length, orig_file_length) =
	loadfn source_file
      in
      let orig_instr_nb = Array.length code in
      let orig_data_nb = Array.length data in
      let orig_prim_nb = Array.length prim in
      print_done ();
      let (code, data) = compress_loop code prim data in
      print_msg "Cleaning primitives... ";
      let prim = Prim.clean code prim in
      print_done ();
      print_msg "Globalise closures... ";
      let (code, data) = Globalise.doit code data in
      print_done ();
      print_msg (Printf.sprintf "Writing `%s'... " !dest_file);
      let (new_code_length, new_data_length, new_prim_length, new_file_length) =
	exportfn !dest_file code prim data begn dumps
      in
      print_done ();
      let new_instr_nb = Array.length code in
      let new_data_nb = Array.length data in
      let new_prim_nb = Array.length prim in
      let gain o n = if o = n then 1. else (float_of_int o /. float_of_int n) in
	print_msg (Printf.sprintf "\
Statistics:\n\
  Instruction number:  %7d  -> %7d   (/%1.2f)\n\
  Code segment length: %7d  -> %7d   (/%1.2f)\n\
  Global data number:  %7d  -> %7d   (/%1.2f)\n\
  Data segment length: %7d  -> %7d   (/%1.2f)\n\
  Primitive number:    %7d  -> %7d   (/%1.2f)\n\
  Prim segment length: %7d  -> %7d   (/%1.2f)\n\
  File length:         %7d  -> %7d   (/%1.2f)\n\
"
  orig_instr_nb new_instr_nb (gain orig_instr_nb new_instr_nb)
  orig_code_length new_code_length (gain orig_code_length new_code_length)
  orig_data_nb new_data_nb (gain orig_data_nb new_data_nb)
  orig_data_length new_data_length (gain orig_data_length new_data_length)
  orig_prim_nb new_prim_nb (gain orig_prim_nb new_prim_nb)
  orig_prim_length new_prim_length (gain orig_prim_length new_prim_length)
  orig_file_length new_file_length (gain orig_file_length new_file_length)
		  );
    with
      | Sys_error msg | Index.Exn msg -> error msg
      | Code.Exn msg | Instr.Exn msg | Data.Exn msg | Dump.Exn msg 
      | Prim.Exn msg ->
	  Printf.eprintf "Error: %s\n" msg;
	  exit 1;
  in
 match Filename.check_suffix source_file ".c" with
    |true -> compile Loader.load_c Linker.export_c
    |false -> compile Loader.load Linker.export

;;
