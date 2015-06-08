type md = {name : string ; mutable content : (string ref) list}

type processors = (md -> unit) list

let quote = Str.regexp "^\\( \\)?\\( \\)?\\( \\)?>\\( \\)?"
let quote_char = [' ';'>']

let ul_li = Str.regexp "^\\( \\)?\\( \\)?\\( \\)?[*+-]\\( \\)?"

let ol_li = Str.regexp "^\\( \\)?\\( \\)?\\( \\)?[0-9]+[.)]\\( \\)?"

let indented_code = Str.regexp "^\\(    \\)"

let fenced_code = Str.regexp "^\\(```\\)\\|\\(~~~\\)\\(.*\\)?"

let rec read_line_to_md_aux ic =
	let rec read_paragraph ic =
		let line = input_line ic in
		let l = String.length line in
		let rl = (in_channel_length ic) - (pos_in ic) in
		if l == 0 then "" else if rl==0 then line else line ^ "\n" ^ (read_paragraph ic)
	in
	if pos_in ic != in_channel_length ic then begin
		let p = ref (read_paragraph ic) in
		p::(read_line_to_md_aux ic)
	end else
		[]

let read_file_to_md f_name = 
	let ic = open_in f_name in
	read_line_to_md_aux ic

let md_fread f_name =
	try let cont = read_file_to_md f_name in 
		{name = f_name ; content =  cont}
	with Sys_error err -> 
		print_string "Could not open file";
		{name = String.sub f_name 0 (String.length f_name - 3); content =  []};;

let rec print_list_to_string l acc = 
	match l with
	| [] -> acc
	| hd::tl -> let to_print = if tl != [] then (!hd ^ "\n") else !hd in print_list_to_string tl (acc^to_print);;

let md_print_to_string markdown = print_list_to_string markdown.content "";;

let md_print markdown oc = let to_print = md_print_to_string markdown in output_string oc to_print;;

let md_pretty_print markdown oc = 
	let raw = md_print_to_string markdown in
	let html = 
	"<!DOCTYPE html>
	    <html>
	      <head>
	        <title>" ^ markdown.name ^ "</title>
	        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
	        <!-- Bootstrap -->
	    	<link href=\"css/bootstrap.min.css\" rel=\"stylesheet\" media=\"screen\">
	      </head>
	      <body><div class=\"container\">" ^ raw ^ "<div></body>
	      <script src=\"http://code.jquery.com/jquery.js\"></script>
	    <script src=\"js/bootstrap.min.js\"></script>
	    </html>"
	in
	output_string oc html;;
	

let md_fwrite markdown print_function filename = let oc = open_out filename in print_function markdown oc

let md_get_content markdown = markdown.content

let init_func str = 
	let tab = Array.of_list (Str.split (Str.regexp ":") str) in
	let str1 = Str.global_replace (Str.regexp "\\n") "\n" tab.(1) in (* solution for newline issue *)
	let reg1 = Str.regexp str1 in
	let reg2 = tab.(0) in
	let temp markdown = 
		let rec temp2 l = 
		match l with
		| h::t -> h := Str.global_replace reg1 reg2 !h;temp2 t
		| [] -> ()
		in
		temp2 markdown.content
	in
	temp;;

let init_inline_procs (conf:string) : processors =
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

let rec proc_inline (markdown:md) (process:processors) : unit =
	match process with
	| hd::tl -> hd markdown;proc_inline markdown tl
	| [] -> ()

let find_indent p c =
	let rec f_i_rec p c k =
		if k == String.length p then k else begin 
			if List.mem p.[k] c then f_i_rec p c (k+1) else k+1
		end
	in
	f_i_rec p c 0


let process_ul (p:string) : string =
	try
	let n = Str.search_forward ul_li p 0 in
	if n == 0 then begin
		let l = Str.split ul_li p in
		let rec p_ul_rec l acc = match l with
		| h::t -> let n_acc = acc ^ "<li>" ^ h ^ "</li>" in p_ul_rec t n_acc
		| [] -> acc
		in
		let p1 = p_ul_rec l "" in
		"<ul>"^p1^"</ul>"
	end else p
	with
	| Not_found -> p

let process_ol (p:string) : string =
	try
	let n = Str.search_forward ol_li p 0 in
	if n == 0 then begin
		let l = Str.split ol_li p in
		let rec p_ul_rec l acc = match l with
		| h::t -> let n_acc = acc ^ "<li>" ^ h ^ "</li>" in p_ul_rec t n_acc
		| [] -> acc
		in
		let p1 = p_ul_rec l "" in
		"<ol>"^p1^"</ol>"
	end else p
	with
	| Not_found -> p
	
let process_quote (p:string) : string =
	try
		let n = Str.search_forward quote p 0 in
		if n == 0 then begin
			let p1 = Str.global_replace quote "" p in
			"<blockquote>"^p1^"</blockquote>"
		end else p
	with
	| Not_found -> p

let process_fenced_code (p:string) : string =
	try
		let n = Str.search_forward fenced_code p 0 in
		let m = Str.search_backward fenced_code p (String.length p - 1) in
		if n == 0 then begin
			let beg = String.index p '\n' in
			let p1 = String.sub p beg (m-beg) in
			"<pre>"^p1^"</pre>"
		end else p
	with
	| Not_found -> p


(* <p> processing, somewhat dirty. Could be improved by keeping in mind which paragraph has already been treated *)
let process_p (p:string) : string =
	let l = String.length p in
	if l == 0 || p == "\n" then "" else begin
		if String.get p 0 != '<' || String.get p (String.length p - 1) != '>' then "<p>"^p^"</p>" else p
	end
	

let proc_from_function f =
	let proc f markdown =
		let rec proc_aux f l =
			match l with
			| h::t -> h:= f !h;proc_aux f t
			| [] -> ()
		in
		proc_aux f markdown.content
	in
	proc f;;

let rec init_paragraph_procs l = 
	match l with
	| h::t -> (proc_from_function h)::(init_paragraph_procs t)
	| [] -> []

let paragraph_procs_list =  [process_quote;process_ul;process_ol;process_fenced_code;process_p]

let proc_paragraph (markdown:md) (l:processors): unit =
	let rec p_p_rec l = match l with
	| h::t -> h markdown; p_p_rec t
	| [] -> ()
	in p_p_rec l

let translate (markdown:md) : unit =
	let inline_procs = init_inline_procs "conf.txt" in
	let paragraph_procs = init_paragraph_procs paragraph_procs_list in
	proc_inline markdown inline_procs;
	proc_paragraph markdown paragraph_procs