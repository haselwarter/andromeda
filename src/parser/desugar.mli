(** Conversion from sugared to desugared input syntax *)

(** A module which holds the desugaring context *)
module Ctx : sig
  (** The type of desugaring context *)
  type t

  (** Empty desugaring context *)
  val empty : t

  (** [included fn ctx] returns true if [fn] has been included in [ctx] *)
  val included : string -> t -> bool

end

(** [toplevel primitive bound c] desugars a toplevel command [c] with a
    list of primitives and their arities, and a list of bound variables
    that are converted to de Bruijn indices. *)
val toplevel : base_dir:string -> Ctx.t -> Input.toplevel -> (Ctx.t * Syntax.toplevel) option
