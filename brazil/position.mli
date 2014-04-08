
(** The type of file positions *)
type t

(** Unknown position *)
val nowhere : t

(** Make a position out of begining and ending position. *)
val make : Lexing.position -> Lexing.position -> t

(** Convert position to a string. *)
val to_string : ?full:bool -> t -> string

