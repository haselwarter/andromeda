(** Judgements can be abstracted *)
type 'a abstraction

(** Judgement that something is a term. *)
type is_term

(** Judgement that something is an atom. *)
type is_atom

(** Judgement that something is a type. *)
type is_type

(** Judgement that something is a type equality. *)
type eq_type

(** Judgement that something is a term equality. *)
type eq_term

(** An argument to a term or type constructor *)
type premise =
  | PremiseIsType of is_type abstraction
  | PremiseIsTerm of is_term abstraction
  | PremiseEqType of eq_type abstraction
  | PremiseEqTerm of eq_term abstraction

(** A stump is obtained when we invert a judgement. *)

type stump_is_type =
  | TypeConstructor of Name.constructor * premise list

type stump_is_term =
  | TermAtom of is_type TT.atom
  | TermConstructor of Name.constructor * premise list
  | TermConvert of is_term * eq_type

type stump_eq_type =
  | EqType of TT.assumption * is_type * is_type

type stump_eq_term =
  | EqTerm of TT.assumption * is_term * is_term * is_type

type 'a stump_abstraction =
  | NotAbstract of 'a
  | Abstract of is_atom * 'a abstraction


(** An auxiliary type for providing arguments to a congruence rule. Each arguments is like
   two endpoints with a path between them, except that no paths between equalities are
   needed. *)
type congruence_premise =
  | CongrIsType of is_type abstraction * is_type abstraction * eq_type abstraction
  | CongrIsTerm of is_term abstraction * is_term abstraction * eq_term abstraction
  | CongrEqType of eq_type abstraction * eq_type abstraction
  | CongrEqTerm of eq_term abstraction * eq_term abstraction


module Signature : sig
  type t

  val empty : t
end

(** Given a type formation rule and a list of premises, match the rule
   against the given premises, make sure they fit the rule, and return the
   judgement corresponding to the conclusion of the rule. *)
val form_is_type_rule : Signature.t -> Name.constructor -> premise list -> is_type

(** Given a term rule and a list of premises, match the rule against the given
   premises, make sure they fit the rule, and return the list of arguments that the term
   constructor should be applied to, together with the natural type of the resulting term.
 *)
val form_is_term_rule : Signature.t -> Name.constructor -> premise list -> is_term

(** Convert atom judgement to term judgement *)
val form_is_term_atom : is_atom -> is_term

val form_is_term_convert : Signature.t -> is_term -> eq_type -> is_term

(** Given an equality type rule and a list of premises, match the rule against the given
   premises, make sure they fit the rule, and return the conclusion of the instance of the rule
   so obtained. *)
val form_eq_type_rule : Signature.t -> Name.constructor -> premise list -> eq_type

(** Given an terms equality type rule and a list of premises, match the rule
   against the given premises, make sure they fit the rule, and return the conclusion of
   the instance of the rule so obtained. *)
val form_eq_term_rule : Signature.t -> Name.constructor -> premise list -> eq_term

val invert_is_type : Signature.t -> is_type -> stump_is_type

val invert_is_term : Signature.t -> is_term -> stump_is_term

val invert_eq_type : Signature.t -> eq_type -> stump_eq_type

val invert_eq_term : Signature.t -> eq_term -> stump_eq_term

val invert_abstraction : (TT.term -> ?lvl:TT.bound -> 'a -> 'a) -> 'a abstraction -> 'a stump_abstraction

(** An error emitted by the nucleus *)
type error

exception Error of error

(** The type judgement of a term judgement. *)
val type_of_term : Signature.t -> is_term -> is_type

(** Typeof for atoms *)
val type_of_atom : is_atom -> is_type

(** Does this atom occur in this judgement, and if so with what type? *)
(* XXX val occurs : is_atom -> is_term -> is_atom option *)

(** Substitution *)

(** [substitute_type t a v] substitutes [v] for [a] in [t]. *)
val substitute_type : is_term -> is_atom -> is_type -> is_type

(** [substitute_term e a v] substitutes [v] for [a] in [e]. *)
val substitute_term : is_term -> is_atom -> is_term -> is_term

(** Destructors *)

(** If [e1 == e2 : A] and [A == B] then [e1 == e2 : B] *)
val convert_eq_term : eq_term -> eq_type -> eq_term

(** Constructors *)

(** Construct the judgment [e == e : A] from [e : A] *)
val reflexivity_term : Signature.t -> is_term -> eq_term

(** Construct the jdugment [A == A] from [A type] *)
val reflexivity_type : is_type -> eq_type

(** Given two terms [e1 : A1] and [e2 : A2] construct [e1 == e2 : A1],
    provided [A1] and [A2] are alpha equal and [e1] and [e2] are alpha equal *)
val mk_alpha_equal_term : Signature.t -> is_term -> is_term -> eq_term option

(** Given two types [A] and [B] construct [A == B] provided the types are alpha equal *)
val mk_alpha_equal_type : is_type -> is_type -> eq_type option

(** Test whether terms are alpha-equal. They may have different types and incompatible contexts even if [true] is returned. *)
val alpha_equal_is_term : is_term -> is_term -> bool

(** Test whether types are alpha-equal. They may have different contexts. *)
val alpha_equal_is_type : is_type -> is_type -> bool

(** If [e1 == e2 : A] then [e2 == e1 : A] *)
val symmetry_term : eq_term -> eq_term

(** If [A == B] then [B == A] *)
val symmetry_type : eq_type -> eq_type

(** If [e1 == e2 : A] and [e2 == e3 : A] then [e1 == e2 : A] *)
val transitivity_term : eq_term -> eq_term -> eq_term

(** If [A == B] and [B == C] then [A == C] *)
val transitivity_type : eq_type -> eq_type -> eq_type

(** Congruence rules *)

val congruence_type_constructor :
  Signature.t -> Name.constructor -> congruence_premise list -> eq_type

val congruence_term_constructor :
  Signature.t -> Name.constructor -> congruence_premise list -> eq_term


(** Print the judgement that something is a term. *)
val print_is_term :
  penv:TT.print_env -> ?max_level:Level.t -> is_term abstraction -> Format.formatter -> unit

(** Print the judgement that something is a type. *)
val print_is_type :
  penv:TT.print_env -> ?max_level:Level.t -> is_type abstraction -> Format.formatter -> unit

(** Print the judgement that terms are equal. *)
val print_eq_term :
  penv:TT.print_env -> ?max_level:Level.t -> eq_term abstraction -> Format.formatter -> unit

(** Print the judgement that types are equal. *)
val print_eq_type :
  penv:TT.print_env -> ?max_level:Level.t -> eq_type abstraction -> Format.formatter -> unit

(** Print a nucleus error *)
val print_error : penv:TT.print_env -> error -> Format.formatter -> unit

module Json :
sig
  val abstraction : ('a -> Json.t) -> 'a abstraction -> Json.t

  val is_term : is_term -> Json.t

  val is_type : is_type -> Json.t

  val eq_term : eq_term -> Json.t

  val eq_type : eq_type -> Json.t
end
