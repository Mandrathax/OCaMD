(*  Markdown  *)

(** basic MarkDown type : name of file + list containing all lines *)
type md

type processors

(** Reads a file and returns a list containing all paragraphs *)
val md_fread : string -> md

(** Writes a file and returns success boolean *)
val md_fwrite : md -> string -> unit

(** prints to output channel *)
val md_print : md -> out_channel -> unit

val init_proc : string -> processors

(** Processes text and replaces *)

val proc : md -> processors -> unit