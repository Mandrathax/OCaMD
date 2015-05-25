type md = {name : string ; mutable content : (string ref) list}

type processors = (md -> unit) list

let rec read_line_to_md_aux ic =
	let rec read_paragraph ic =
		try
			let line = input_line ic in
			let l = String.length line in
			print_int l;
			if l == 0 then "" else line ^ "\r" ^ (read_paragraph ic)
		with End_of_file-> raise End_of_file
	in
	try
		let p = ref (read_paragraph ic) in
		print_string ("\n" ^ !p ^ "\n");
		p::(read_line_to_md_aux ic)
	with End_of_file-> []

let read_file_to_md f_name = 
	let ic = open_in f_name in
	read_line_to_md_aux ic

let md_fread f_name =
	try let cont = read_file_to_md f_name in 
		{name = f_name ; content =  cont}
	with Sys_error err -> 
		print_string "Could not open file";
		{name = String.sub f_name 0 (String.length f_name - 3); content =  []}

let rec print_list l oc = 
	match l with
	| [] -> close_out oc
	| hd::tl -> let to_print = if tl != [] then (!hd ^ "\n") else !hd in
		output oc to_print 0 (String.length to_print); print_list tl oc

let md_print markdown oc = print_list markdown.content oc;;

let md_fwrite markdown filename = let oc = open_out filename in md_print markdown oc

let md_get_content markdown = markdown.content

let init_func str = 
	let tab = Array.of_list (Str.split (Str.regexp ":") str) in
	let str1 = Str.global_replace (Str.regexp "\\n") "\n" tab.(1) in (* solution for newline issue *)
	let str2 = Str.global_replace (Str.regexp "\\n") "\n" tab.(0) in (* solution for newline issue *)
	let reg1 = Str.regexp str1 in
	let reg2 = str2 in
	(* print_string tab.(1);
	print_string tab.(0); *)
	let temp markdown = 
		let rec temp2 l = 
		match l with
		| h::t -> h := Str.global_replace reg1 reg2 !h;temp2 t
		| [] -> ()
		in
		temp2 markdown.content
	in
	temp;;

let init_proc (conf:string) : processors =
	let ic = open_in conf in
	let ret = ref [] in
	try
		while true; do
			let line = input_line ic in
			ret := (init_func line)::!ret;
		done; []
	with End_of_file ->
		close_in ic;
		List.rev !ret;;

let rec proc (markdown:md) (process:processors) : unit =
	match process with
	| hd::tl -> hd markdown;proc markdown tl
	| [] -> ()


(* let rec h3_proc markdown = 
	let l = markdown.content in
	match l with
	| []->()
	| hd::tl -> hd := Str.global_replace (Str.regexp "### \\(\\(.\\)*\\)") "<h3>\\1</h3>" !hd; h3_proc {name = markdown.name ; content = tl} *)