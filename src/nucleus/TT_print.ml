(****** Printing routines *****)

open Jdg_typedefs

let add_forbidden x penv = { penv with forbidden = x :: penv.forbidden }

let rec abstraction
   : 'b . (bound -> 'b -> bool) ->
          (?max_level:Level.t -> penv:print_env -> 'b -> Format.formatter -> unit) ->
          ?max_level:Level.t ->
          penv:print_env ->
          'b abstraction ->
          Format.formatter -> unit
  = fun occurs_v print_v ?max_level ~penv abstr ppf ->
  let rec fold penv xus = function

    | NotAbstract v ->
       let xus = List.rev xus in
       begin match xus with
       | [] ->
          print_v ?max_level ~penv v ppf
       | _::_ ->
         Print.print ~at_level:Level.abstraction ?max_level ppf "@[<hov 2>%t@ %t@]"
           (Print.sequence (binder ~penv) "" xus)
           (print_v ~max_level:Level.abstraction_body ~penv v)
       end

    | Abstract (x, u, abstr) ->
       let x =
         (if TT_occurs.abstraction occurs_v 0 abstr then
            Name.refresh penv.forbidden x
          else
            Name.anonymous ())
       in
       let penv = add_forbidden x penv in
       fold penv ((x,u) :: xus) abstr

  in

  fold penv [] abstr

and term ?max_level ~penv e ppf =
  match e with
  | TermAtom {atom_name=x; _} ->
     Name.print_atom ~printer:(penv.atoms) x ppf

  | TermBound k -> Name.print_debruijn penv.forbidden k ppf

  | TermConstructor (c, args) ->
     constructor ?max_level ~penv c args ppf

  | TermMeta (mv, args) ->
     meta ?max_level ~penv mv args ppf

  | TermConvert (e, _, _) -> term ~penv ?max_level e ppf

and ty ?max_level ~penv t ppf =
  match t with

  | TypeConstructor (c, args) ->
     constructor ?max_level ~penv c args ppf

  | TypeMeta (mv, args) ->
     meta ?max_level ~penv mv args ppf

and eq_type ?max_level ~penv (EqType (_asmp, t1, t2)) ppf =
  (* TODO: print _asmp? *)
  Print.print
    ?max_level
    ~at_level:Level.eq
    ppf
    "@[<hov>%t@]@ %s@ @[<hov>%t@]"
    (ty ~penv t1)
    (Print.char_equal ())
    (ty ~penv t2)

and eq_term ?max_level ~penv (EqTerm (_asmp, e1, e2, t)) ppf =
  (* TODO: print _asmp? *)
  Print.print
    ?max_level
    ~at_level:Level.eq
    ppf
    "@[<hov>%t@]@ %s@ @[<hov>%t@]@ :@ @[<hov>%t@]"
    (term ~penv e1)
    (Print.char_equal ())
    (term ~penv e2)
    (ty ~penv t)

and meta :
  type a . ?max_level:Level.t -> penv:print_env
            -> a meta -> term list -> Format.formatter -> unit
  = fun ?max_level ~penv {meta_name;_} args ppf ->
  match args with
  | [] ->
     Name.print_meta ~parentheses:true ~printer:penv.metas meta_name ppf
  | _::_ ->
     Print.print ~at_level:Level.meta ?max_level ppf "@[<hov 2>%t@ %t@]"
    (Name.print_meta ~printer:penv.metas meta_name)
    (Print.sequence (term ~max_level:Level.meta_arg ~penv) "" args) ;

and constructor ?max_level ~penv c args ppf =
  match args with
  | [] ->
     Name.print_ident ~parentheses:true c ppf
  | _::_ ->
     Print.print ~at_level:Level.constructor ?max_level ppf "@[<hov 2>%t@ %t@]"
       (Name.print_ident c)
       (Print.sequence (argument ~penv) "" args) ;

and argument ~penv arg ppf =
  match arg with
  | ArgumentIsType abstr ->
     abstraction TT_occurs.ty ty ~max_level:Level.constructor_arg ~penv abstr ppf
  | ArgumentIsTerm abstr ->
     abstraction TT_occurs.term term ~max_level:Level.constructor_arg ~penv abstr ppf
  | ArgumentEqType abstr ->
     abstraction TT_occurs.eq_type eq_type ~max_level:Level.constructor_arg ~penv abstr ppf
  | ArgumentEqTerm abstr ->
     abstraction TT_occurs.eq_term eq_term ~max_level:Level.constructor_arg ~penv abstr ppf


and binder ~penv (x,t) ppf =
  Print.print ppf "{%t@ :@ %t}"
    (Name.print_ident ~parentheses:true x)
    (ty ~penv t)



let error ~penv err ppf =
  let open Jdg_typedefs in
  match err with
  | InvalidInstantiation -> Format.fprintf ppf "invalid instantiation"
  | InvalidAbstraction -> Format.fprintf ppf "invalid abstraction"
  | TooFewArguments -> Format.fprintf ppf "too few arguments"
  | TooManyArguments -> Format.fprintf ppf "too many arguments"
  | TermExpected -> Format.fprintf ppf "term expected"
  | TypeExpected -> Format.fprintf ppf "type expected"
  | ExtraAssumptions -> Format.fprintf ppf "extra assumptions"
  | InvalidApplication -> Format.fprintf ppf "invalid application"
  | InvalidArgument -> Format.fprintf ppf "invalid argument"
  | IsTypeExpected -> Format.fprintf ppf "type argument expected"
  | IsTermExpected -> Format.fprintf ppf "term argument expected"
  | EqTypeExpected -> Format.fprintf ppf "type equality argument expected"
  | EqTermExpected -> Format.fprintf ppf "term equality argument expected"
  | AbstractionExpected -> Format.fprintf ppf "abstraction expected"
  | InvalidSubstitution -> Format.fprintf ppf "invalid substutition"
  | InvalidCongruence -> Format.fprintf ppf "invalid congruence argument"

  | InvalidConvert (t1, t2) ->
     Format.fprintf ppf "Trying to convert something at@ %t@ using an equality on@ %t@."
                    (ty ~penv t1) (ty ~penv t2)

  | AlphaEqualTypeMismatch (t1, t2) ->
     Format.fprintf ppf "The types@ %t@ and@ %t@ should be alpha equal."
                    (ty ~penv t1) (ty ~penv t2)

  | AlphaEqualTermMismatch (e1, e2) ->
     Format.fprintf ppf "The terms@ %t@ and@ %t@ should be alpha equal."
                    (term ~penv e1) (term ~penv e2)

