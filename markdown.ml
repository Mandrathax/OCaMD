
type md = {name : string ; mutable content : string list}

let read_file_to_list f_name = 
	let c = ref [] in
	let ic = open_in f_name in
	try
		while true; do
			let line = input_line ic in
			c := line::!c
		done; []
	with End_of_file ->
		close_in ic;
		List.rev !c

let md_fread f_name =
	try let cont = read_file_to_list f_name in 
		{name = f_name ; content = cont}
	with Sys_error err -> 
		print_string "Could not open file";
		{name = String.sub f_name 0 (String.length f_name - 3); content = []}

let rec print_list l oc = 
	match l with
	| [] -> close_out oc
	| hd::tl -> let to_print = if tl != [] then (hd ^ "\n") else hd in
		output oc to_print 0 (String.length to_print); print_list tl oc

let md_print markdown oc = print_list markdown.content oc;;

let md_fwrite markdown filename = let oc = open_out filename in md_print markdown oc

let md_get_content markdown = markdown.content





