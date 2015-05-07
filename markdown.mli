(*  Markdown  *)

(** basic MarkDown type : name of file + list containing all lines *)
type md

(** Reads a file and returns a list containing all paragraphs *)
val md_fread : string -> md

(** Writes a file and returns success boolean *)
val md_fwrite : md -> string -> unit

(** Gets raw list of content from md *)
val md_get_content : md -> string list

(** prints to output channel *)
val md_print : md -> out_channel -> unit
