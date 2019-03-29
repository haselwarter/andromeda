(** Conversion to JSON *)

open Nucleus_types

let assumptions { free ; is_type_meta ; is_term_meta ; eq_type_meta ; eq_term_meta ; bound } =
  let free =
    if Nonce.map_is_empty free
    then []
    else [("free", Nonce.Json.map free)]
  and is_type_meta =
    if Nonce.map_is_empty is_type_meta
    then []
    else [("is_type_meta", Nonce.Json.map is_type_meta)]
  and is_term_meta =
    if Nonce.map_is_empty is_term_meta
    then []
    else [("is_term_meta", Nonce.Json.map is_term_meta)]
  and eq_type_meta =
    if Nonce.map_is_empty eq_type_meta
    then []
    else [("eq_type_meta", Nonce.Json.map eq_type_meta)]
  and eq_term_meta =
    if Nonce.map_is_empty eq_term_meta
    then []
    else [("eq_term_meta", Nonce.Json.map eq_term_meta)]
  and bound =
    if Bound_set.is_empty bound
    then []
    else [("bound", Json.List (List.map (fun k -> Json.Int k) (Bound_set.elements bound)))]
  in
  Json.record (free @ is_type_meta @ is_term_meta @ eq_type_meta @ eq_term_meta @ bound)


let rec is_term e =
  let e =
    match e with

    | TermAtom {atom_nonce=x; atom_type=t} -> Json.tag "TermAtom" [Nonce.Json.nonce x; is_type t]

    | TermBound b -> Json.tag "TermBound" [Json.Int b]

    | TermConstructor (c, lst) -> Json.tag "TermConstructor" (Ident.Json.ident c :: args lst)

    | TermMeta ({meta_nonce; _}, lst) ->
       Json.tag "TermMeta" (Nonce.Json.nonce meta_nonce :: (List.map is_term lst))

    | TermConvert (e, asmp, t) -> Json.tag "TermConvert" [is_term e; assumptions asmp; is_type t]

  in Json.tag "IsTerm" [e]

and is_type t =
  let t =
    match t with
    | TypeConstructor (c, lst) -> Json.tag "TypeConstructor" (Ident.Json.ident c :: args lst)
    | TypeMeta ({meta_nonce; _}, lst) ->
       Json.tag "TypeMeta" (Nonce.Json.nonce meta_nonce :: (List.map is_term lst))
  in Json.tag "IsType" [t]

and args lst =
  (List.map
     (function
       | ArgumentIsTerm abstr -> Json.tag "ArgumentIsTerm" (abstraction is_term [] abstr)
       | ArgumentIsType abstr -> Json.tag "ArgumentIsType" (abstraction is_type [] abstr)
       | ArgumentEqType _ -> Json.tag "ArgumentIsType" []
       | ArgumentEqTerm _ -> Json.tag "ArgumentEqTerm" [])
     lst)

and abstraction : 'a . ('a -> Json.t) -> (Name.t * is_type) list -> 'a abstraction -> Json.t list =
  fun json_u xts ->
    function
    | Abstract ({atom_nonce=x; atom_type=t}, abstr) ->
       abstraction json_u ((Nonce.name x, t) :: xts) abstr
    | NotAbstract u ->
       let xts = List.map (fun (x, t) -> Json.List [Name.Json.name x; is_type t]) (List.rev xts) in
       [Json.tuple xts ; json_u u]


let rec abstraction json_u = function
  | NotAbstract u -> Json.tag "NotAbstract" [json_u u]
  | Abstract ({atom_nonce=x; atom_type=t}, abstr) ->
     Json.tag "Abstract" [Nonce.Json.nonce x; is_type t; abstraction json_u abstr]

let eq_term (EqTerm (asmp, e1, e2, t)) =
  Json.tag "EqTerm" [assumptions asmp; is_term e1; is_term e2; is_type t]

let eq_type (EqType (asmp, t1, t2)) =
  Json.tag "EqType" [assumptions asmp; is_type t1; is_type t2]