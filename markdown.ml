
type md = {name : string ; mutable content : string}

type processors = (md -> unit) list

let read_file_to_string f_name = 
	let c =  ref "" in
	let ic = open_in f_name in
	try
		while true; do
			let line = input_line ic in
			if !c == "" then c := line else c := !c ^ "\n" ^ line
		done; ""
	with End_of_file ->
		close_in ic;
		!c

let md_fread f_name =
	try let cont = read_file_to_string f_name in 
		{name = f_name ; content =  cont}
	with Sys_error err -> 
		print_string "Could not open file";
		{name = String.sub f_name 0 (String.length f_name - 3); content =  ""}

let rec print_list l oc = 
	match l with
	| [] -> close_out oc
	| hd::tl -> let to_print = if tl != [] then (!hd ^ "\n") else !hd in
		output oc to_print 0 (String.length to_print); print_list tl oc

let md_print markdown oc = output oc markdown.content 0 (String.length markdown.content);;

let md_fwrite markdown filename = let oc = open_out filename in md_print markdown oc

let md_get_content markdown = markdown.content

let init_func str = 
	let tab = Array.of_list (Str.split (Str.regexp ":") str) in
	let reg1 = Str.regexp tab.(1) in
	let reg2 = tab.(0) in
	print_string tab.(1);
	print_string tab.(0);
	fun markdown -> markdown.content <- Str.global_replace reg1 reg2 markdown.content;;

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

let rec proc (markdown:md) (process:processors) =
	match process with
	| hd::tl -> hd markdown;proc markdown tl
	| [] -> ()


(* let rec h3_proc markdown = 
	let l = markdown.content in
	match l with
	| []->()
	| hd::tl -> hd := Str.global_replace (Str.regexp "### \\(\\(.\\)*\\)") "<h3>\\1</h3>" !hd; h3_proc {name = markdown.name ; content = tl} *)