let filename = "test.md"

open Markdown
let () = 
	print_string "+---------------+\n";
	print_string "| Testing OCaMD |\n";
	print_string "+---------------+\n\n";
	let test_md = md_fread filename in
	md_fwrite test_md "output_test.txt"
