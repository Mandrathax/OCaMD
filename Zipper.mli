val encode : Huffman.encoding_dictionary -> Huffman.plain_text -> Huffman.cipher_text

val decode : Huffman.decoding_dictionary -> Huffman.cipher_text -> Huffman.plain_text

val compress : string -> unit

val decompress : string -> unit
