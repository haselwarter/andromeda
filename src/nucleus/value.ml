type ty_judge = Tt.ty * AtomSet.t

(** A judgement [e : t] where [e] is guaranteed to have the type [t]. *)
type term_judge = Tt.term * ty_judge * AtomSet.t

(** Runtime values and results *)
type value =
  | Judge of term_judge
  | Closure of closure
  | Handler of handler

(** A closure *)
and closure = value -> result

(** Possible results of evaluating a computation. *)
and result =
  | Return of value
  | Operation of string * value * closure

and handler = {
  handler_val: closure option;
  handler_ops: (string * (value -> value -> result)) list;
  handler_finally: closure option;
}

(** The monadic bind [bind r f] feeds the result [r : result]
    into function [f : value -> 'a]. *)
let rec bind r f =
  match r with
  | Return v -> f v
  | Operation (op, v, k) -> Operation (op, v, fun x -> (bind (k x) f))

let print_dependencies ?max_level atoms ppf =
  if AtomSet.is_empty atoms then
    Print.print ppf ""
  else
    Print.print ppf "@ {%t}"
                (Print.sequence Name.print_atom "," (AtomSet.elements atoms))

let print_judge ?max_level xs (e,t,atoms) ppf =
  Print.print ~at_level:0 ppf "@[<hov 2>%t@\n    : %t%t@]"
              (Tt.print_term ~max_level:999 xs e)
              (Tt.print_ty ~max_level:999 xs t)
              (print_dependencies ~max_level:999 atoms)

let print_closure ?max_level xs _ ppf =
  Print.print ~at_level:0 ppf "<function>"

let print_handler ?max_level xs h ppf =
  Print.print ~at_level:0 ppf "<handler>" (* XXX improve in your spare time *)

let print ?max_level xs v ppf =
  match v with
  | Judge j -> print_judge ?max_level xs j ppf
  | Closure f -> print_closure ?max_level xs f ppf
  | Handler h -> print_handler ?max_level xs h ppf

let as_judge ~loc = function
  | Judge j -> j
  | Closure _ -> Error.runtime ~loc "expected a judgment but got a function"
  | Handler _ -> Error.runtime ~loc "expected a judgment but got a handler"

let as_closure ~loc = function
  | Judge j -> Error.runtime ~loc "expected a function but got a judgement %t" (print_judge [] j)
  | Closure f -> f
  | Handler _ -> Error.runtime ~loc "expected a function but got a handler"

let as_handler ~loc = function
  | Judge j -> Error.runtime ~loc "expected a handler but got a judgement %t" (print_judge [] j)
  | Closure _ -> Error.runtime ~loc "expected a handler but got a function"
  | Handler h -> h

let return_judge e t atoms = Return (Judge (e, t, atoms))

let to_value ~loc = function
  | Return v -> v
  | Operation (op, _, _) ->
     Error.runtime ~loc "unhandled operation %t" (Name.print_op op)
