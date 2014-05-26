(********************)
(* Helper Functions *)
(********************)

let print_ty ctx t =
  Print.ty (Context.names ctx) t

let print_term ctx term =
  Print.term (Context.names ctx) term

(* It's important to eta-expand the next two functions, because they're
 * expensive, and we don't want to do all the administrative computation
 * if debugging output has been disabled (so that this function will never
 * be applied to a ppf argument).
 *)

let print_pattern ctx k p ppf =
  let rec names i =
    if i < k then ("?" ^ string_of_int i) :: names (i + 1) else Context.names ctx
  in
  let rec inst i =
    if i <= k then (i, Syntax.mkVar i) :: inst (i+1) else []
  in
  let p = Pattern.shift k 0 p in
  let e = (match Pattern.subst_term (inst 0) 0 p with Pattern.Term e -> e | _ -> assert false) in
    Print.term (names 0) e ppf

let print_pattern_ty ctx k p ppf =
  let rec names i =
    if i < k then ("?" ^ string_of_int i) :: names (i + 1) else Context.names ctx
  in
  let rec inst i =
    if i <= k then (i, Syntax.mkVar i) :: inst (i+1) else []
  in
  let p = Pattern.shift_ty k 0 p in
  let t = (match Pattern.subst_ty (inst 0) 0 p with Pattern.Ty t -> t | _ -> assert false) in
    Print.ty (names 0) t ppf


(** Signal that pattern matching failed. *)
exception Mismatch

(** Should we suppress failure messages? *)
let tentative = ref false

let tentatively f =
  let old = ! tentative  in
  let _ = tentative := true  in
  let answer = f()  in
  tentative := old;
  answer

(***********)
(* type_of *)
(***********)



let rec type_of ctx (exp, loc) =
  match exp with
  | Syntax.Equation (_, _, body)
  | Syntax.Rewrite (_, _, body) -> type_of ctx body
  | Syntax.Ascribe (_, ty) -> ty
  | Syntax.Lambda (x, t1, t2, _) -> Syntax.mkProd ~loc x t1 t2
  | Syntax.UnitTerm -> Syntax.mkUnit ~loc ()
  | Syntax.Idpath (t, e) -> Syntax.mkPaths ~loc t e e
  | Syntax.J (_, (_, _, _, u), _, e2, e3, e4) -> Syntax.strengthen_ty u [e2; e3; e4]
  | Syntax.Refl (t, e) -> Syntax.mkId ~loc t e e
  | Syntax.Coerce (_, beta, _) -> Syntax.mkUniverse ~loc beta
  | Syntax.NameUnit -> Syntax.mkUniverse ~loc Universe.zero
  | Syntax.NameProd (alpha, beta, _, _, _) -> Syntax.mkUniverse ~loc (Universe.max alpha beta)
  | Syntax.NameUniverse alpha -> Syntax.mkUniverse ~loc (Universe.succ alpha)
  | Syntax.NamePaths (alpha, _, _, _)
  | Syntax.NameId    (alpha, _, _, _) -> Syntax.mkUniverse ~loc alpha


  | Syntax.App (annot, e1, e2) ->
      let (_, _, t2) = infer_app_annot "type_of" loc ctx annot e1 e2  in
      Syntax.beta_ty t2 e2

  | Syntax.Var v ->
      begin
        (* Trick to make sure that whnf can preserve type_of.
         * WARNING: This will work as long as we don't allow variables without
         * definitions to suddenly acquire definitions later on. Or at least,
         * acquire a definition with an equivalent but distinct type_of. *)
        (*
        match Context.lookup_def v ctx with
          | None    -> *) Context.lookup_var v ctx
          (*| Some e' -> type_of ctx e'*)
      end

and infer_app_annot caller loc ctx annot e1 _e2 =
  begin
    match annot with
    | Some triple -> triple
    | None ->
        failwith "YYY: infer_app_annot";
        (*
        begin
          let t1 = type_of ctx e1  in
          match as_prod' ~use_rws:false ctx t1  with
            | Some triple -> triple
            | None -> Error.typing ~loc
                          "%s: Cannot reconstruct App annotation for term"
                          caller (print_term ctx e1)
        end
  *)
  end


and mkApp ?(loc=Position.nowhere) ctx x t1 t2 e1 e2 =
  begin
    match as_prod' ~use_rws:false ctx (type_of ctx e1) with
    | None -> Syntax.mkApp x t1 t2 e1 e2
    | Some (_, t1', t2') ->
        if equal_ty' ~use_eqs:false ~use_rws:false ctx t1 t1' &&
           equal_ty' ~use_eqs:false ~use_rws:false
                         (Context.add_var x t1 ctx) t2 t2'  then
              Syntax.mkApp_unsafe None e1 e2
        else
          Syntax.mkApp x t1 t2 e1 e2
  end

(*************************)
(* Weak-Head Normalizing *)
(*************************)

and whnf_ty ~use_rws ctx ((t',loc) as t) =
  let whnf = whnf ~use_rws in
  let whnf_ty = whnf_ty ~use_rws in
  begin match t' with

    (* tynorm-el *)
    | Syntax.El (alpha, e) ->
      begin match fst (whnf ctx (Syntax.mkUniverse ~loc alpha) e) with

        (* tynorm-pi *)
        | Syntax.NameProd (beta, gamma, x, e1, e2)
            when Universe.eq alpha (Universe.max beta gamma) ->
          let t1 = Syntax.mkEl ~loc:(snd e1) beta e1 in
          let t2 = Syntax.mkEl ~loc:(snd e2) gamma e2 in
          Syntax.mkProd ~loc x t1 t2

        (* tynorm-unit *)
        | Syntax.NameUnit ->
          Syntax.mkUnit ~loc ()

        (* tynorm-universe *)
        | Syntax.NameUniverse beta
            when Universe.eq alpha (Universe.succ beta) ->
          Syntax.mkUniverse ~loc beta

        (* tynorm-coerce *)
        | Syntax.Coerce (beta, gamma, e)
            when Universe.eq alpha gamma ->
          whnf_ty ctx (Syntax.mkEl ~loc:(snd e) beta e)

        (* tynorm-paths *)
        | Syntax.NamePaths (beta, e1, e2, e3)
            when Universe.eq alpha beta ->
          let t1 = Syntax.mkEl ~loc:(snd e1) alpha e1  in
            Syntax.mkPaths ~loc t1 e2 e3

        (* tynorm-id *)
        | Syntax.NameId (beta, e1, e2, e3)
            when Universe.eq alpha beta ->
          let t1 = Syntax.mkEl ~loc:(snd e1) alpha e1  in
            Syntax.mkId ~loc t1 e2 e3

        (* tynorm-other *)
        | (Syntax.Var _ | Syntax.Equation _ | Syntax.Rewrite _ | Syntax.Ascribe _
              | Syntax.Lambda _ | Syntax.App _ | Syntax.UnitTerm | Syntax.Idpath _
              | Syntax.J _ | Syntax.Refl _ | Syntax.Coerce _ | Syntax.NameProd _
              | Syntax.NameUniverse _ | Syntax.NamePaths _ | Syntax.NameId _) as e' ->
          Syntax.mkEl ~loc alpha (e', loc)
      end

    | (Syntax.Universe _ | Syntax.Unit | Syntax.Prod _ | Syntax.Paths _ | Syntax.Id _) ->
      t
  end

and whnf ~use_rws ctx t ((e',loc) as e0) =
  let equal_ty' = equal_ty' ~use_eqs:false ~use_rws
  and whnf = whnf ~use_rws
  in
  let e =
    begin match e' with

      (* norm-var-def *)
      | Syntax.Var k ->
        begin match Context.lookup_def k ctx with
          | None -> e0
          | Some e' -> whnf ctx t e'
        end

      (* norm-equation *)
      | Syntax.Equation (e1, t1, e2) ->
        let h = as_hint' ~use_rws ctx e1 t1 in
          whnf (Context.add_equation h ctx) t e2

      (* norm-rewrite *)
      | Syntax.Rewrite (e1, t1, e2)  ->
        let h = as_hint' ~use_rws ctx e1 t1 in
          whnf (Context.add_rewrite h ctx) t e2

      (* norm-ascribe *)
      | Syntax.Ascribe(e, _) ->
        whnf ctx t e

      (* XXX: We should be able to do a little better in the case
       * of an App with no annotation... *)

      | Syntax.App (annot, e1, e2) ->
        begin
          let x, u1, u2 = infer_app_annot "whnf/app" loc ctx annot e1 e2  in
          let e1 = whnf ctx (Syntax.mkProd ~loc x u1 u2) e1 in
            match fst e1 with
              (* norm-app-beta *)
              | Syntax.Lambda (y, t1, t2, e1')
                  when tentatively (fun () -> equal_ty' ctx u1 t1 &&
                                              equal_ty' (Context.add_var x u1 ctx) u2 t2) ->
                whnf ctx (Syntax.beta_ty u2 e2) (Syntax.beta e1' e2)

              (* norm-app-other *)
              | _ ->
                Syntax.mkApp ~loc x u1 u2 e1 e2
        end

      | Syntax.J (t, (x,y,p,u), (z,e1), e2, e3, e4) ->
        begin
          let e2 = whnf ctx (Syntax.mkPaths ~loc t e3 e4) e2 in
            match fst e2 with
              (* norm-J-idpath *)
              | Syntax.Idpath (t', e2')
                  when tentatively (fun () -> equal_ty' ctx t t') ->
                whnf ctx (Syntax.betas_ty u [e2; e2'; e2]) (Syntax.beta e1 e2')

              (* norm-J-other *)
              | _ ->
                Syntax.mkJ ~loc t (x,y,p,u) (z,e1) e2 e3 e4
        end

      (* norm-coerce-trivial *)
      | Syntax.Coerce (alpha, beta, e)
          when Universe.eq alpha beta ->
        whnf ctx (Syntax.mkUniverse ~loc alpha) e

      | Syntax.Coerce (alpha, beta, e) ->
        begin match whnf ctx (Syntax.mkUniverse ~loc alpha) e with
          (* norm-coerce-trans *)
          | (Syntax.Coerce (gamma, delta, e), _) when Universe.eq delta alpha ->
            if Universe.eq gamma beta
            then
              (* norm-coerce-trivial *)
              e
            else
              Syntax.mkCoerce ~loc gamma beta e

          (* norm-coerce-other *)
          | e ->
            Syntax.mkCoerce ~loc alpha beta e
        end

      | (Syntax.Lambda _ | Syntax.UnitTerm | Syntax.Idpath _ |
         Syntax.Refl _ | Syntax.NameUnit | Syntax.NameProd _ |
         Syntax.NameUniverse _ | Syntax.NamePaths _ | Syntax.NameId _) ->
        e0
    end
  in
    let answer =
          if use_rws
          then rewrite_term ctx e t
          else e
    in
    begin
      (*
      if (Syntax.equal answer e0) then
            Print.debug "Term %t was head-normal" (print_term ctx e0)
          else
            Print.debug "Rewrote %t to %t" (print_term ctx e0) (print_term ctx answer);
            *)
      answer
    end


(** [rewrite_term ctx e t] rewrites term [e] of type [t] using rewrite hints
    from [ctx]. After rewriting it re-runs weak head-normalization on the
    resulting term. *)

and rewrite_term ctx e t =
  (*Print.debug "@[<hv 4>rewrite_term %d:@ %t@]"                 *)
  (*    (List.length (Context.rewrites ctx)) (print_term ctx e) ;*)

  let match_hint k pt pe1 pe2 =
    (*Print.debug "@[<hv 2>match_hint considering if@ %t  matches pattern@ %t at@ %t@]"*)
    (*    (print_term ctx e)                                                           *)
    (*    (print_pattern ctx k pe1)                                                    *)
    (*    (print_pattern_ty ctx k pt) ;                                                *)

    let inst = []  in
    (*let inst = new_match_term ctx inst k pe1 pt e t  in*)
    let inst =  match_term ~magenta:true k inst 0 ctx pe1 e t  in
    (*let _ = Print.debug "match_hint: instantiation succeeded" in*)
    let pe2 = Pattern.subst_term inst 0 pe2  in
    match pe2 with
    | Pattern.Term e2 ->
       begin
         (* XXX: This is *not* sufficient to detect uninstantiated variables;
          * only uninstantiated variables used in the right-hand-side.
          * We really need to examine the instantiation. Maybe compare
          * Length.list inst and k? *)

         (*Print.debug "Success! Hint rewrote to %t" (print_term ctx e2);*)
         e2
       end
    | _ ->
       begin
         (*Print.debug "Match succeeded, but there were uninstantiated variables";*)
         (* XXX  Per Jason, backtrack instead of failing here *)
         raise Mismatch
       end
  in
  let rec match_hints = function
    | [] ->
      e
    | (k, pt, pe1, pe2) :: hs ->
      begin try
        (*Print.debug "considering rewriting %t to %t"            *)
        (*    (print_pattern ctx k pe1) (print_pattern ctx k pe2);*)
        let e2 = match_hint k pt pe1 pe2 in
        (*Print.debug "@[<hv 2>rewrote@ %t at@ %t@;<1 -2>to@ %t@;<1 -2> using@ %t and@ %t@]"*)
        (*    (print_term ctx e) (print_ty ctx t) (print_term ctx e2)                       *)
        (*    (print_pattern ctx k pe1) (print_pattern ctx k pe2) ;                         *)
        whnf ~use_rws:true ctx t e2
        with
          | Mismatch ->
              (*Print.debug "nope";*)
                match_hints hs
          (*| Error.Error (_,s1,s2) -> (Print.debug "unexpected Error %s %s" s1 s2; match_hints hs)*)
          | ex -> (Print.debug "unexpected exception %s"
                        (Printexc.to_string ex); match_hints hs)
      end
  in
  let hs = Context.rewrites ctx in
  let answer = match_hints hs  in
  (*let _ = Print.debug "rewrite_term returned %t" (print_term ctx answer) in*)
  answer


(** See if terms [e1] and [e2] of type [t] are equal by an equality hint. *)
and equal_by_equation ctx t e1 e2 =
  (*Print.debug "equal_by_equation? %t@ and %t"*)
  (*   (print_term ctx e1) (print_term ctx e2);*)
  let match_hint k pt pe1 pe2 =
    (*Print.debug "considering hint %t = %t"                *)
    (*  (print_pattern ctx k pe1) (print_pattern ctx k pe2);*)
    let inst = []  in
    (* Match the left-hand-side and incorporate results into the right-hand-side *)
    (*let inst = new_match_term ctx inst k pe1 pt e1 t  in*)
    let inst =  match_term ~magenta:true k inst 0 ctx pe1 e1 t  in
    let pt = Pattern.subst_ty inst 0 pt
    and pe2 = Pattern.subst_term inst 0 pe2 in

    (* Match the right-hand-side *)
    (*let inst = new_match_term ctx inst k pe2 pt e2 t  in*)
    let inst =  match_term ~magenta:true k [] 0 ctx pe2 e2 t  in

    (* Instantiate and check *)
    let pt = Pattern.subst_ty inst 0 pt
    and pe1 = Pattern.subst_term inst 0 pe1
    and pe2 = Pattern.subst_term inst 0 pe2 in
      begin match pt, pe1, pe2 with
        | Pattern.Ty t', Pattern.Term e1', Pattern.Term e2' ->
          (* Until someone proves that pattern matching works, keep the assert
             around *)
          (*assert (equal_ty' ~use_eqs:false ~use_rws:false ctx t t' &&*)
             (*equal_term ~use_eqs:false ~use_rws:false ctx e1 e1' t &&*)
             (*equal_term ~use_eqs:false ~use_rws:false ctx e2 e2' t);*)
          ()
        | _ -> raise Mismatch
      end
  in
  let rec match_hints = function
    | [] -> false
    | (k, pt, pe1, pe2) :: hs ->
      begin try
        match_hint k pt pe1 pe2 ; true
        with
          | Mismatch -> match_hints hs
      end
  in
    match_hints (Context.equations ctx)

(* Equality of types *)
and equal_ty' ~use_rws ~use_eqs ctx t u =
  Print.debug "equal_ty'@ %t@ %t" (print_ty ctx t) (print_ty ctx u);

  (* chk-tyeq-refl *)
  (Syntax.equal_ty t u)

  ||

  let t = whnf_ty ~use_rws ctx t  in
  let u = whnf_ty ~use_rws ctx u  in
  equal_whnf_ty ~use_eqs ~use_rws ctx t u

(* equality of weak-head-normal types *)
and equal_whnf_ty ~use_eqs ~use_rws ctx ((t', tloc) as t) ((u', uloc) as u) =
  let equal_ty' = equal_ty' ~use_eqs ~use_rws
  and equal_term = equal_term ~use_eqs ~use_rws
  in
  begin
    match t', u' with

    (* chk-tyeq-path-refl *)
    | _, _ when Syntax.equal_ty t u ->
        true

    (* chk-tyeq-prod *)
    | Syntax.Prod(x, t1, t2), Syntax.Prod(_, u1, u2) ->
        equal_ty' ctx t1 u1 &&
        equal_ty' (Context.add_var x t1 ctx) t2 u2

    (* chk-tyeq-paths *)
    | Syntax.Paths(t,e1,e2), Syntax.Paths(u,e1',e2') ->
        equal_ty' ctx t u &&
        equal_term ctx e1 e1' t &&
        equal_term ctx e2 e2' t

    (* chk-tyeq-id *)
    | Syntax.Id(t,e1,e2), Syntax.Id(u,e1',e2') ->
        equal_ty' ctx t u &&
        equal_term ctx e1 e1' t &&
        equal_term ctx e2 e2' t

    | Syntax.El _, _
    | _, Syntax.El _ ->
        begin match Syntax.name_of t, Syntax.name_of u with
          (* chk-tyeq-el *)
          | Some (e1, alpha), Some (e2, beta) ->
            Universe.eq alpha beta &&
            equal_term ctx e1 e2 (Syntax.mkUniverse ~loc:(snd t) alpha)
          | (_, None) | (None, _) -> false
        end

    | (Syntax.Universe _ | Syntax.Unit
       | Syntax.Prod _ | Syntax.Paths _ | Syntax.Id _), _ ->
           (if ((!(Print.verbosity)) >= 3 || not (!tentative)) then
             Print.warning "@[<hv 2>Why are types@ %t@;<1 -2>and@ %t@;<1 -2>equal?@]"
                  (print_ty ctx t) (print_ty ctx u));
           false
  end

(* Equality of terms.

   Precondition: t is well-formed
                 e1 : t
                 e2 : t
 *)
and equal_term ~use_eqs ~use_rws ctx e1 e2 t =

  (*if (not (!tentative)) then*)
    (*Print.debug "@[<hv 4>equal_term %b %b:@ %t@;<1 -4> and@ %t@]" *)
    (*      use_eqs use_rws (print_term ctx e1) (print_term ctx e2);*)

  (* chk-eq-refl *)
  (Syntax.equal e1 e2)

  ||

  (* chk-eq-hint *)
  (use_eqs && (equal_by_equation ctx t e1 e2 || equal_by_equation ctx t e2 e1))

  ||
  begin
    let t' = whnf_ty ~use_rws ctx t in
    equal_ext ~use_eqs ~use_rws ctx e1 e2 t'
  end


(* Equality of terms at a weak-head-normal type.

   Precondition: ty is well-formed *and weak-head-normal*
                 e1 : ty
                 e2 : ty
 *)
and equal_ext ~use_eqs ~use_rws ctx ((_, loc1) as e1) ((_, loc2) as e2) ((t', _) as t) =
  begin
      (*Print.debug "@[<hv 4>equal_ext %b %b:@ %t@;<1 -4> and@ %t@ at %t@]"*)
      (*      use_eqs use_rws (print_term ctx e1) (print_term ctx e2)      *)
      (*      (print_ty ctx t);                                            *)
    match t' with

    (* chk-eq-ext-prod *)
    | Syntax.Prod (x, t, u) ->
        (* To keep the two x binders straight, we'll call the one in
           the context z. *)
        let ctx' = Context.add_var x t ctx  in   (* ctx' === ctx, z *)
                                           (* ctx       |- e1  : ... *)
        let e1' = Syntax.weaken 0 e1 in    (* ctx, z    |- e1' : ... *)
                                           (* ctx       |- e2  : ... *)
        let e2' = Syntax.weaken 0 e2 in    (* ctx, z    |- e2' : ... *)
                                           (* ctx       |- t  type *)
        let t'  = Syntax.weaken_ty 0 t in  (* ctx, z    |- t' type *)
                                           (* ctx,    x |- u  type *)
        let u' = Syntax.weaken_ty 1 u  in  (* ctx, z, x |- u' type *)
        let z = Syntax.mkVar 0  in         (* ctx, z    |- z : ... *)
        equal_term ~use_eqs ~use_rws ctx'
              (Syntax.mkApp ~loc:loc1 x t' u' e1' z) (* XXX Annotations might not elim *)
              (Syntax.mkApp ~loc:loc2 x t' u' e2' z) (* XXX Annotations might not elim *)
              u

    (* chk-eq-ext-unit *)
    | Syntax.Unit ->
        true

    (* chk-eq-ext-K *)
    | Syntax.Id (_, _, _) ->
        true

    (* chk-eq-ext-whnf *)
      | Syntax.Universe _ | Syntax.El _ | Syntax.Paths _ ->
        let e1' = whnf ~use_rws ctx t e1 in
        let e2' = whnf ~use_rws ctx t e2  in
        equal_whnf ~use_eqs ~use_rws ctx e1' e2' t
  end

(* Equality of two weak-head-normal terms.

   Precondition: e1 : t
                 e2 : t
 *)
and equal_whnf ~use_eqs ~use_rws ctx ((e1', loc1) as e1) ((e2', loc2) as e2) t =
      (*Print.debug "@[<hv 4>equal_whnf %b %b:@ %t@;<1 -4> and@ %t@ at %t@]"*)
      (*      use_eqs use_rws (print_term ctx e1) (print_term ctx e2)       *)
      (*      (print_ty ctx t);                                             *)
  let equal_ty' = equal_ty' ~use_eqs ~use_rws
  and equal_term = equal_term ~use_eqs ~use_rws
  in
  begin
    match e1', e2' with

    (* chk-eq-whnf-reflexivity *)
    | _, _ when Syntax.equal e1 e2 ->
        true

    (* chk-eq-whnf-equation *)
    | _, _ when use_eqs && equal_by_equation ctx t e1 e2 ->
        true

    (* chk-eq-whnf-var *)
    | Syntax.Var i1, Syntax.Var i2 -> i1 = i2

    (* chk-eq-whnf-app *)
    | Syntax.App(annot1, ef1, ex1), Syntax.App(annot2, ef2, ex2) ->
        let x, t1, u1 = infer_app_annot "equal_whnf/left" loc1 ctx annot1 ef1 ex1 in
        let _, t2, u2 = infer_app_annot "equal_whnf/right" loc2 ctx annot2 ef2 ex2 in
        if tentatively (fun () -> equal_ty' ctx t1 t2
                         && equal_ty' (Context.add_var x t1 ctx) u1 u2
                         && equal_whnf ~use_eqs ~use_rws ctx ef1 ef2
                                       (Syntax.mkProd ~loc:loc1 x t1 u1)) then
           equal_term ctx ex1 ex2 t1
        else
          let e1'' = Syntax.simplify e1  in
          let e2'' = Syntax.simplify e2  in
          if (Syntax.equal e1 e1'' && Syntax.equal e2 e2'') then
            begin
             ((if (not (!tentative)) then
                 Print.warning "@[<hv 2>Why are applications@ %t@;<1 -2>and@ %t@;<1 -2>equal?@]"
                      (print_term ctx e1) (print_term ctx e2));
             false)

            end
          else
            (* Hail Mary equivalence check *)
            equal_term ctx e1'' e2'' t

    (* chk-eq-whnf-idpath *)
    | Syntax.Idpath(t, e1), Syntax.Idpath(u, e2) ->
        equal_ty' ctx t u && equal_term ctx e1 e2 t

    (* chk-eq-whnf-j *)
    | Syntax.J (t, (x,y,p,u), (z, e1), e2, e3, e4),
      Syntax.J (t', (_,_,_,u'), (_, e1'), e2', e3', e4') ->
      let ctx_xyp, ctx_z = Context.for_J t x y p z ctx in
      let e1_ty_expected =
                                                      (* ctx,    x, y, p |- u type *)
          let v = Syntax.weaken_ty 3 u                (* ctx, z, x, y, p |- v type *)
                                                      (* ctx             |- t type *)
          and s = Syntax.weaken_ty 0 t                (* ctx, z           |- s type *)
          and zvar = Syntax.mkVar 0                   (* ctx, z |- z *)
          in
            (* ctx, z |- v[z/x,z/y,(idpath z)/p] type *)
            Syntax.strengthen_ty v
              [zvar; zvar; Syntax.mkIdpath s zvar]

      in

        (*
        let j_ty_expected =
          Syntax.strengthen_ty u [e3; e4; e2]  in       (* ctx |- u[e3/x,e4/y,e2/p] *)
        *)

        equal_ty' ctx t t'
        && equal_ty' ctx_xyp u u'
        && equal_term ctx_z e1 e1' e1_ty_expected
        && equal_term ctx e3 e3' t
        && equal_term ctx e4 e4' t
        && equal_whnf ~use_eqs ~use_rws ctx e2 e2' (Syntax.mkPaths ~loc:loc1 t e3 e4)

    (* chk-eq-whnf-refl *)
    | Syntax.Refl(t, e1), Syntax.Refl(u, e2) ->
        equal_ty' ctx t u && equal_term ctx e1 e2 t

    (* chk-eq-whnf-prod *)
    | Syntax.NameProd (alpha, beta, x, e1, e2),
      Syntax.NameProd (alpha', beta', _, e1', e2') ->
        Universe.eq alpha alpha' && Universe.eq beta beta'
        && equal_term ctx e1 e1' (Syntax.mkUniverse alpha)
        && equal_term (Context.add_var x (Syntax.mkEl alpha e1) ctx)
                 e2 e2' (Syntax.mkUniverse beta)

    (* chk-eq-whnf-universe *)
    | Syntax.NameUniverse alpha, Syntax.NameUniverse beta ->
        Universe.eq alpha beta

    (* chk-eq-whnf-unit *)              (** Subsumed by reflexivity check! *)
    (*| Syntax.NameUnit, Syntax.NameUnit -> true *)

    (* chk-eq-whnf-paths *)
    | Syntax.NamePaths(alpha, e1, e2, e3), Syntax.NamePaths(alpha', e1', e2', e3') ->
        Universe.eq alpha alpha'
        && equal_term ctx e1 e1' (Syntax.mkUniverse alpha)
        && equal_term ctx e2 e2' (Syntax.mkEl alpha e1)
        && equal_term ctx e3 e3' (Syntax.mkEl alpha e1)

    (* chk-eq-whnf-id *)
    | Syntax.NameId(alpha, e1, e2, e3), Syntax.NameId(alpha', e1', e2', e3') ->
        Universe.eq alpha alpha'
        && equal_term ctx e1 e1' (Syntax.mkUniverse alpha)
        && equal_term ctx e2 e2' (Syntax.mkEl alpha e1)
        && equal_term ctx e3 e3' (Syntax.mkEl alpha e1)

    (* chk-eq-whnf-coerce *)
    | Syntax.Coerce(alpha, _beta, e1), Syntax.Coerce(alpha', _beta', e1') ->
        Universe.eq alpha alpha'
        && equal_term ctx e1 e1' (Syntax.mkUniverse alpha)

    (* chk-eq-whnf-abs *)
    | Syntax.Lambda(x,t1,t2,e1), Syntax.Lambda(_,u1,u2,e2) ->
        equal_ty' ctx t1 u1
        && let ctx' = Context.add_var x t1 ctx  in
           equal_ty' ctx' t2 u2 && equal_term ctx' e1 e2 t2

    (* chk-eq-whnf-unit-right *)
    | _, Syntax.UnitTerm ->
        true

    (* chk-eq-whnf-unit-left *)
    | Syntax.UnitTerm, _ ->
        true

    (* chk-eq-whnf-refl-left *)
    | Syntax.Refl _, _ ->
        true

    (* chk-eq-whnf-refl-right *)
    | _, Syntax.Refl _ ->
        true

    | (Syntax.Var _ | Syntax.Equation _ | Syntax.Rewrite _ | Syntax.Ascribe _
      | Syntax.Lambda _ | Syntax.App _ | Syntax.Idpath _
      | Syntax.J _ | Syntax.Coerce _ | Syntax.NameUnit
      | Syntax.NameProd _ | Syntax.NameUniverse _ | Syntax.NamePaths _
      | Syntax.NameId _), _ ->
          let e1'' = Syntax.simplify e1  in
          let e2'' = Syntax.simplify e2  in
          if (Syntax.equal e1 e1'' && Syntax.equal e2 e2'') then
           ((if ((!(Print.verbosity)) >= 3 || not (!tentative)) then
               Print.warning "@[<hv 2>Why are terms@ %t@;<1 -2>and@ %t@;<1 -2>equal?@]"
                    (print_term ctx e1) (print_term ctx e2));

            false)
          else
            equal_term ctx e1'' e2'' t
  end

and expand_all_annotations ctx =
  let expand_transf bvs term =
    match term with
    | Syntax.App(None, e1, e2), loc ->
        let x, t1, t2 = infer_app_annot "expand_all_annotations" loc ctx None e1 e2  in
        Syntax.mkApp ~loc x t1 t2 e1 e2
    | _ -> term
  in
    Syntax.transform expand_transf 0

and as_hint' ~use_rws ctx (_, loc) t =
  let rec collect ctx' u =
    match fst (whnf_ty ~use_rws ctx' u) with
      | Syntax.Prod (x, t1, t2) ->
        let (k, t, e1, e2) = collect (Context.add_var x t1 ctx') t2 in
          (k + 1, t, e1, e2)
      | Syntax.Id (t, e1, e2) -> (0, t, e1, e2)
      | Syntax.Universe _ | Syntax.El _ | Syntax.Unit | Syntax.Paths _ ->
        Error.typing ~loc "this expression cannot be used as an equality hint, its type is %t"
          (print_ty ctx t)
  in
  let (k, t, e1, e2) = collect ctx t in
  let pt = Pattern.of_ty k t in
  let pe1 = Pattern.of_term k (expand_all_annotations ctx e1) in
  let pe2 = Pattern.of_term k (expand_all_annotations ctx e2) in
    (k, pt, pe1, pe2)

(* Simple matching of a type pattern against a type. *)
and match_ty ~magenta k inst l ctx pt ((t',loc) as t) =
  let pt = (match inst with [] -> pt | _ -> Pattern.subst_ty inst l pt) in
  (*Print.debug "match_ty: type %t, pat %t" (print_ty ctx t) (print_pattern_ty ctx k pt);*)

  let match_term = match_term ~magenta k
  and match_magenta = if magenta then match_ty ~magenta k else (fun inst _ _ _ _ -> inst)
  and match_ty = match_ty ~magenta k
  in
  match pt with

    | Pattern.Ty u  ->
      if equal_ty' ~use_eqs:false ~use_rws:false ctx t u
      then inst
      else raise Mismatch

    | Pattern.El (alpha, pe) ->
      begin match Syntax.name_of t with
        | None -> raise Mismatch
        | Some (e', beta) ->
          if Universe.eq alpha beta then
            let t = Syntax.mkUniverse ~loc alpha in
              match_term inst l ctx pe e' t
          else
            inst
      end

    | Pattern.Prod (_, pt1, pt2) ->
      begin match as_prod' ~use_rws:false ctx t with
        | None -> raise Mismatch
        | Some (x, t1, t2) ->
          let inst = match_ty inst l ctx pt1 t1 in
          let inst = match_ty inst (l+1) (Context.add_var x t1 ctx) pt2 t2 in
            inst
      end

    | Pattern.Paths (pt, pe1, pe2) ->
      begin match as_paths' ~use_rws:false ctx t with
        | None -> raise Mismatch
        | Some (t, e1, e2) ->
          let inst = match_magenta inst l ctx pt t in
          let inst = match_term inst l ctx pe1 e1 t in
          let inst = match_term inst l ctx pe2 e2 t in
            inst
      end

    | Pattern.Id (pt, pe1, pe2) ->
      begin match as_id' ~use_rws:false ctx t with
        | None -> raise Mismatch
        | Some (t, e1, e2) ->
          let inst = match_magenta inst l ctx pt t in
          let inst = match_term inst l ctx pe1 e1 t in
          let inst = match_term inst l ctx pe2 e2 t in
            inst
      end

(* Simple matching of a term pattern against a term. *)
and match_term ~magenta k inst l ctx p e t =
  let p = (match inst with [] -> p | _ -> Pattern.subst_term inst l p)  in
  (*Print.debug "match_term, term %t,@ pat %t"   *)
  (*  (print_term ctx e) (print_pattern ctx k p);*)
  let match_term = match_term ~magenta k
  and match_magenta = if magenta then match_ty ~magenta k else (fun inst _ _ _ _ -> inst)
  and match_ty = match_ty ~magenta k
  in
  match p with

  | Pattern.Term e' ->
    if Syntax.equal e' e
    then inst
    else raise Mismatch

  | Pattern.PVar i ->
    begin
      try
        (*Print.debug "PVar: i = %d, depth = %d, l = %d, e = %t"        *)
        (*   i  (List.length (Context.names ctx)) l  (print_term ctx e);*)
        let e' = List.assoc i inst in
        let e'  = Syntax.shift l e'  in
        (*let _ = Print.debug "Repeat on pattern variable %d = %t"*)
        (*              i (print_term ctx e')  in                 *)
        if equal_term ~use_eqs:false ~use_rws:false ctx e' e t
        then inst
        else raise Mismatch
      with
        | Not_found ->
            let e  = Syntax.shift ~exn:Mismatch (- l)  e  in
            (i,e) :: inst
    end

  | Pattern.Lambda (_, pt1, pt2, pe) ->
    begin match fst e with
      | Syntax.Lambda (x, t1, t2, e) ->
        let inst = match_ty inst l ctx pt1 t1 in
        let ctx' = Context.add_var x t1 ctx in
        let inst = match_magenta inst (l+1) ctx' pt2 t2 in
        let inst = match_term inst (l+1) ctx' pe e t2 in
          inst
      | _ -> raise Mismatch
    end

  | Pattern.App ((_, pt1, pt2), pe1, pe2) ->
    begin match fst e with
      | Syntax.App (annot, e1, e2) ->
        let x, t1, t2 = infer_app_annot "match_term/App" (snd e) ctx annot e1 e2  in
        (* We need to match the function part first, since in
           the case of a spine it probably sets metavariables
           (including type families) that occur in the type. *)
        let inst = match_term inst l ctx pe1 e1 (Syntax.mkProd x t1 t2) in
        let inst = match_magenta inst l ctx pt1 t1 in
        let inst = match_magenta inst (l+1) (Context.add_var x t1 ctx) pt2 t2 in
        let inst = match_term inst l ctx pe2 e2 t1 in
          inst
      | _ -> raise Mismatch
    end

  | Pattern.Idpath (pt, pe) ->
    begin match fst e with
      | Syntax.Idpath (t, e) ->
        let inst = match_magenta inst l ctx pt t in
        let inst = match_term inst l ctx pe e t in
          inst
      | _ -> raise Mismatch
    end

  | Pattern.J (pt, (_,_,_,pu), (_,pe1), pe2, pe3, pe4) ->
    begin match fst e with
      | Syntax.J (t, (x,y,p,u), (z,e1), e2, e3, e4) ->
        let inst = match_magenta inst l ctx pt t in
        let ctx_xyp, ctx_z = Context.for_J t x y p z ctx in
        let inst = match_ty inst (l+3) ctx_xyp pu u in
        let inst = match_term inst (l+1) ctx_z pe1 e1 t in
        let inst = match_term inst l ctx pe2 e2 t in
        (* XXX strictly speaking, [e3] and [e4] are magenta, so we could skip them *)
        let inst = match_term inst l ctx pe3 e3 t in
        let inst = match_term inst l ctx pe4 e4 t in
          inst
      | _ -> raise Mismatch
    end

  | Pattern.Refl (pt, pe) ->
    begin match fst e with
      | Syntax.Refl (t, e) ->
        let inst = match_magenta inst l ctx pt t in
        let inst = match_term inst l ctx pe e t in
          inst
      | _ -> raise Mismatch
    end

   (** XXX should switch to comparing type names *)

  | Pattern.Coerce (alpha, beta, pe) ->
    begin match fst e with
      | Syntax.Coerce (gamma, delta, e)
          when Universe.eq alpha gamma && Universe.eq beta delta ->
        let inst = match_term inst l ctx pe e (Syntax.mkUniverse alpha) in
          inst
      | _ -> raise Mismatch
    end

  | Pattern.NameProd (alpha, beta, _, pe1, pe2) ->
    begin match fst e with
      | Syntax.NameProd (gamma, delta, x, e1, e2)
          when Universe.eq alpha gamma && Universe.eq beta delta ->
        let inst = match_term inst l ctx pe1 e1 (Syntax.mkUniverse gamma) in
        let inst =
          match_term
            inst
            (l+1)
            (Context.add_var x (Syntax.mkEl gamma e1) ctx)
            pe2
            e2
            (Syntax.mkUniverse delta)
        in
          inst
      | _ -> raise Mismatch
    end

  | Pattern.NamePaths (alpha, pe1, pe2, pe3) ->
    begin match fst e with
      | Syntax.NamePaths (beta, e1, e2, e3)
          when Universe.eq alpha beta ->
        let inst = match_term inst l ctx pe1 e1 (Syntax.mkUniverse beta) in
        let inst = match_term inst l ctx pe2 e1 (Syntax.mkEl beta e1) in
        let inst = match_term inst l ctx pe3 e1 (Syntax.mkEl beta e1) in
          inst
      | _ -> raise Mismatch
    end

  | Pattern.NameId (alpha, pe1, pe2, pe3) ->
    begin match fst e with
      | Syntax.NameId (beta, e1, e2, e3)
          when Universe.eq alpha beta ->
        let inst = match_term inst l ctx pe1 e1 (Syntax.mkUniverse beta) in
        let inst = match_term inst l ctx pe2 e1 (Syntax.mkEl beta e1) in
        let inst = match_term inst l ctx pe3 e1 (Syntax.mkEl beta e1) in
          inst
      | _ -> raise Mismatch
    end


and as_prod' ~use_rws ctx t =
  match fst (whnf_ty ~use_rws ctx t) with

    | Syntax.Prod (x, t1, t2) ->
      Some (x, t1, t2)

    | Syntax.Universe _ | Syntax.El _ | Syntax.Unit | Syntax.Paths _ | Syntax.Id _ ->
      None

and as_universe' ~use_rws ctx t =
  match fst (whnf_ty ~use_rws ctx t) with

    | Syntax.Universe alpha ->
      Some alpha

    | Syntax.El _ | Syntax.Unit | Syntax.Prod _ | Syntax.Paths _ | Syntax.Id _ ->
        None

and as_paths' ~use_rws ctx t =
  match fst (whnf_ty ~use_rws ctx t) with

    | Syntax.Paths (t, e1, e2) ->
      Some (t, e1, e2)

    | Syntax.Universe _ | Syntax.El _ | Syntax.Unit | Syntax.Prod _ | Syntax.Id _ ->
      None

and as_id' ~use_rws ctx t =
  match fst (whnf_ty ~use_rws ctx t) with

    | Syntax.Id (t, e1, e2) ->
      Some (t, e1, e2)

    | Syntax.Universe _ | Syntax.El _ | Syntax.Unit | Syntax.Prod _ | Syntax.Paths _ ->
      None

and norm_ty ctx ((t', loc) as t) : Syntax.ty =
  let answer =
  match t' with

  (* norm-ty-universe *)
  | Syntax.Universe _ -> t

  | Syntax.El (alpha, e) ->
    begin match norm ctx e with

      (* norm-ty-el-coerce *)
      | (Syntax.Coerce (beta, gamma, e), _) ->
        norm_ty ctx  (Syntax.mkEl ~loc beta e)

      (* norm-ty-el-unit *)
      | (Syntax.NameUnit, _) ->
        Syntax.mkUnit ~loc ()

      (* norm-ty-el-prod *)
      | (Syntax.NameProd (beta, gamma, x, e1, e2), _) ->
        let t1 = norm_ty ctx (Syntax.mkEl ~loc:(snd e1) beta e1)  in
        let ctx' = Context.add_var x t1 ctx in
        let t2 = norm_ty ctx' (Syntax.mkEl ~loc:(snd e2) gamma e2)  in
        Syntax.mkProd ~loc x t1 t2

      (* norm-ty-el-universe *)
      | (Syntax.NameUniverse beta, _) ->
        Syntax.mkUniverse ~loc beta

      (* norm-ty-el-paths *)
      | (Syntax.NamePaths (beta, e1, e2, e3), _) ->
        let t1 = norm_ty ctx (Syntax.mkEl ~loc alpha e1) in
        let e2' = norm ctx e2 in
        let e3' = norm ctx e3 in
        Syntax.mkPaths ~loc t1 e2' e3'

      (* norm-ty-el-id *)
      | (Syntax.NameId (beta, e1, e2, e3), _) ->
        let t1 = norm_ty ctx (Syntax.mkEl ~loc alpha e1) in
        let e2' = norm ctx e2 in
        let e3' = norm ctx e3 in
        Syntax.mkId ~loc t1 e2' e3'

      (* norm-ty-el-other *)
      | e ->
        Syntax.mkEl ~loc alpha e
    end

  (* norm-ty-unit *)
  | Syntax.Unit -> t

  (* norm-ty-prod *)
  | Syntax.Prod (x, t, u) ->
    let t' = norm_ty ctx t in
    let ctx' = Context.add_var x t ctx in
    let u' = norm_ty ctx' u in
    Syntax.mkProd ~loc x t' u'

  (* norm-ty-paths *)
  | Syntax.Paths (t, e1, e2) ->
    let t' = norm_ty ctx t in
    let e1' = norm ctx e1 in
    let e2' = norm ctx e2 in
    Syntax.mkPaths ~loc t' e1' e2'

  (* norm-ty-id *)
  | Syntax.Id (t, e1, e2) ->
    let t' = norm_ty ctx t in
    let e1' = norm ctx e1 in
    let e2' = norm ctx e2 in
    Syntax.mkId ~loc t' e1' e2'

  in
    let _ = if (not (Syntax.equal_ty t answer)) then
       Print.debug "Normalizing type %t gave %t"
          (print_ty ctx t) (print_ty ctx answer)  in
    answer


and norm ctx ((e', loc) as e) : Syntax.term =
  let answer =
  match e' with
    (* norm-var *)
    | Syntax.Var k ->
        begin match Context.lookup_def k ctx with
          | None -> e
          | Some e' -> norm ctx e'
        end

    (* norm-equation *)
    | Syntax.Equation (_e1, (_e2, _e3), e4) ->
      norm ctx e4

    (* norm-rewrite *)
    | Syntax.Rewrite (_e1, (_e2, _e3), e4) ->
      norm ctx e4

    (* norm-ascribe *)
    | Syntax.Ascribe (e, _t) ->
      norm ctx e

    (* norm-star *)
    | Syntax.UnitTerm -> e

    (* norm-abs *)
    | Syntax.Lambda (x, t1, t2, e) ->
      let t1' = norm_ty ctx t1 in
      let ctx = Context.add_var x t1 ctx  in
      let t2' = norm_ty ctx t2 in
      let e' = norm ctx e in
      Syntax.mkLambda ~loc x t1' t2' e'

    | Syntax.App (annot, e1, e2) ->
      let x, t1, t2 = infer_app_annot "norm/App" loc ctx annot e1 e2  in
      let t1' = norm_ty ctx t1 in
      let t2' = norm_ty (Context.add_var x t1 ctx) t2 in
        begin match norm ctx e1 with

          (* norm-app-redex *)
          | (Syntax.Lambda (_, u1, u2, e1'), _)
              when Syntax.equal_ty t1' u1 && Syntax.equal_ty t2' u2 ->
            let e2' = norm ctx e2 in
              norm ctx (Syntax.beta e1' e2')

          (* norm-app-other *)
          | e1' ->
            let e2' = norm ctx e2 in
            Syntax.mkApp ~loc x t1' t2' e1' e2'
        end

    (* norm-idpath *)
    | Syntax.Idpath (t, e) ->
      let t' = norm_ty ctx t in
      let e' = norm ctx e in
      Syntax.mkIdpath ~loc t' e'

    (* norm-refl *)
    | Syntax.Refl (t, e) ->
      let t' = norm_ty ctx t in
      let e' = norm ctx e in
      Syntax.mkRefl ~loc t' e'

    | Syntax.J (t, (x, y, p, u), (z, e1), e2, e3, e4) ->
      let t' = norm_ty ctx t in
        begin match norm ctx e2 with

          (* norm-j-redex *)
          | (Syntax.Idpath (t'', e2'), _)
               when Syntax.equal_ty t' t'' ->
             let ctx_z = Context.add_var z t ctx  in
             let e1' = norm ctx_z e1 in
               norm ctx (Syntax.beta e1' e2')

          (* norm-j-other *)
          | e2' ->
            let ctx_xy = Context.add_vars [x, t; y, t] ctx  in
            let ctx_xyp = Context.add_var
                  p (Syntax.mkPaths t (Syntax.mkVar 1) (Syntax.mkVar 2)) ctx_xy  in
            let u' = norm_ty ctx_xyp u in
            let e1' = norm ctx e1 in
            let e3' = norm ctx e3 in
            let e4' = norm ctx e4 in
            Syntax.mkJ ~loc t' (x, y, p, u') (z, e1') e2' e3' e4'
        end

    | Syntax.Coerce (alpha, beta, e) ->
      begin match norm ctx e with

        (* norm-coerce-trivial *)
        | e' when Universe.eq alpha beta -> e'

        (* norm-coerce-trans *)
        | (Syntax.Coerce (gamma, delta, e'), loc)
            when Universe.eq alpha delta ->
          norm ctx (Syntax.mkCoerce ~loc gamma beta e')

        (* norm-coerce-pi *)
        | (Syntax.NameProd (gamma, delta, x, e1, e2), loc)
            when Universe.eq alpha (Universe.max gamma delta) &&
                 Universe.leq gamma alpha && Universe.leq delta alpha ->
          let e1' = norm ctx (Syntax.mkCoerce ~loc:(snd e1) gamma beta e1) in
          let ctx' = Context.add_var x (Syntax.mkUniverse gamma) ctx in
          let e2' = norm ctx' (Syntax.mkCoerce ~loc:(snd e2) delta beta e2) in
          Syntax.mkNameProd ~loc beta beta x e1' e2'

        (* norm-coerce-paths *)
        | (Syntax.NamePaths (gamma, e1, e2, e3), loc)
            when Universe.eq alpha gamma ->
          let e1' = norm ctx (Syntax.mkCoerce ~loc:(snd e1) alpha beta e1) in
          let e2' = norm ctx e2 in
          let e3' = norm ctx e3 in
          Syntax.mkNamePaths ~loc beta e1' e2' e3'

        (* norm-coerce-id *)
        | (Syntax.NameId (gamma, e1, e2, e3), loc)
            when Universe.eq alpha gamma ->
          let e1' = norm ctx (Syntax.mkCoerce ~loc:(snd e1) alpha beta e1) in
          let e2' = norm ctx e2 in
          let e3' = norm ctx e3 in
          Syntax.mkNameId ~loc beta e1' e2' e3'

        (* name-coerce-other *)
        | e' ->
          Syntax.mkCoerce ~loc alpha beta e'
      end

    (* norm-name-unit *)
    | Syntax.NameUnit -> e

    (* norm-name-prod *)
    | Syntax.NameProd (alpha, beta, x, e1, e2) ->
      let e1' = norm ctx e1 in
      let ctx' = Context.add_var x (Syntax.mkUniverse alpha) ctx in
      let e2' = norm ctx' e2 in
      Syntax.mkNameProd ~loc alpha beta x e1' e2'

    (* norm-name-universe *)
    | Syntax.NameUniverse _ -> e

    (* norm-name-paths *)
    | Syntax.NamePaths (alpha, e1, e2, e3) ->
      let e1' = norm ctx e1 in
      let e2' = norm ctx e2 in
      let e3' = norm ctx e3 in
      Syntax.mkNamePaths ~loc alpha e1' e2' e3'

    (* norm-name-id *)
    | Syntax.NameId (alpha, e1, e2, e3) ->
      let e1' = norm ctx e1 in
      let e2' = norm ctx e2 in
      let e3' = norm ctx e3 in
      Syntax.mkNameId ~loc alpha e1' e2' e3'
  in
  let _ =
    if (not (Syntax.equal e answer)) then
      Print.debug "Recursive normalization of %t returned %t"
               (print_term ctx e) (print_term ctx answer)  in
  let rw = rewrite_term ctx answer (type_of ctx e)  in
  let _ =
    if (not (Syntax.equal answer rw)) then
      Print.debug "Rewriting %t gave us %t"
               (print_term ctx answer) (print_term ctx rw)  in
  if Syntax.equal answer rw then
    answer
  else
    norm ctx rw



let equal_ty = equal_ty' ~use_eqs:true ~use_rws:true

let as_prod = as_prod' ~use_rws:true
let as_paths = as_paths' ~use_rws:true
let as_id = as_id' ~use_rws:true
let as_universe = as_universe' ~use_rws:true
let as_hint = as_hint' ~use_rws:true


