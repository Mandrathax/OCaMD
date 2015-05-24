let filename = "test.md"

open Markdown
let () = 
	print_string "+---------------+\n";
	print_string "| Testing OCaMD |\n";
	print_string "+---------------+\n\n";
	let test_md = md_fread filename in
	let procs = init_proc "conf.txt" in
	proc test_md procs;
	(* print_string (md_get_content test_md); *)
	md_fwrite test_md "output_test.html"
