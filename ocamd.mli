(*  Markdown  *)

(** Wrapper for text file *)
type md

(** List of functions processing the file *)
type processors

(** gets name of md file *)
val md_get_name : md -> string

(** Reads a file and returns a list containing all paragraphs *)
val md_fread : string -> md

(** Writes a file and returns success boolean *)
val md_fwrite : md -> (md -> out_channel -> unit) -> string -> unit

(** prints to output channel *)
val md_print : md -> out_channel -> unit

(** prints to output channel with a twitter bootstrap overlay *)
val md_pretty_print : md -> out_channel -> unit

(** produces processors for inline conversion (ie *x* -> <em>x</em> like conversion), with first parameter being the name of the conf file *)
val init_inline_procs : string -> processors

(** produces processors for paragraph conversion  with first parameter being a list of transformation function *)
val init_paragraph_procs : (string -> string) list -> processors

(** Processes text and replaces *)
val proc_inline : md -> processors -> unit
val proc_paragraph : md -> processors -> unit

(** Transforms md into html *)
val translate : md -> unit