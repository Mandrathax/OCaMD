let file_2_text filename =
	let in_stream = LazyIO.read_file filename in
	Huffman.to_plain_text in_stream

let rec map_2 f txt = lazy (
    match Lazy.force txt with
    | Nil ->
        Nil
    | Cons (x, xs) ->
        concat (f x) (map_2 f xs)
  	)

let encode txt dic =
	let enc = encode_symbol dic in
	map_2 enc txt

let decode dic enc_txt =
	let dec = decode_symbol dic in
	map dec txt

let compress filename =
	let txt = file_2_text filename in
	let dic = Huffman.dictionaries_from_text txt in
	match dic with
	| (s,e_d,d_d) -> 
	let enc_stream = encode txt e_d in
	let enc_stream = Pack.push_int_as_bits s (Sys.word_size) enc_stream in
	let bytes = ByteStream.from_bits enc_stream in
	let bytes = Huffman.push_decoding_dictionary d_d bytes in
	LazyIO.write_file (filename^".zz") bytes

let decompress filename =
	let enc_bytes = read_file filename in
	let popped = Huffman.pop_decoding_dictionary enc_bytes in
	match popped with
		| (d_d,e_t) -> 
		let enc_stream = ByteStream.to_bits e_t in
		let popped2 = Pack.pop_bits_as_int Sys.word_size enc_stream in
		match popped_2 with
			| (s,e_s) -> 
			let	txt = decode d_d e_s in
			let out_stream = Huffman.from_plain_text txt in
			LazyIO.write_file filename out_stream