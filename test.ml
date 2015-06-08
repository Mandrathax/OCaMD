let filename = "test.md"

open Ocamd
let () = 
	print_string "+---------------+\n";
	print_string "| Testing OCaMD |\n";
	print_string "+---------------+\n\n";
	let test_md = md_fread filename in
	let t = Sys.time () in
	translate test_md;
	Printf.printf "Execution time : %fs" (Sys.time () -. t);
	md_fwrite test_md md_pretty_print "output_test.html"
