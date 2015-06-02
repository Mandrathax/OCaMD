let filename = "test.md"

open Ocamd
let () = 
	print_string "+---------------+\n";
	print_string "| Testing OCaMD |\n";
	print_string "+---------------+\n\n";
	let test_md = md_fread filename in
	translate test_md;
	(* print_string (md_get_content test_md); *)
	md_fwrite test_md md_pretty_print "output_test.html"
