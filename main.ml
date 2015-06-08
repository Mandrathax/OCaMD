open Ocamd

let usage = "Usage :\n\n\tocamd [input_file] ([output_file], optional)"

let false_args () = print_string "Wrong nb of arguments\n"; print_string usage
let file_not_found f = print_string ("File "^f^" not found"); print_string usage

let () =
let arg = Sys.argv in
let l = Array.length arg in
if l < 2 || l > 3 then false_args () else begin
	if Sys.file_exists arg.(1) then begin
		let markdown = md_fread arg.(1) in
		translate markdown;
		let output_file = if l==3 then arg.(2) else (md_get_name markdown) ^ ".html" in
		md_fwrite markdown md_pretty_print output_file
	end else file_not_found arg.(1)
end

