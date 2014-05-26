(** Lazy pattern matching, to figure out which rewrite rules apply. *)

type name = string

type universe = Universe.t

type ty =
  | Ty of Syntax.ty
  | El of universe * term
  | Prod of name * ty * ty
  | Paths of ty * term * term
  | Id of ty * term * term

and term =
  | Term of Syntax.term
  | PVar of int
  | Lambda of name * ty * ty * term
  | App of (name * ty * ty) * term * term
  | Idpath of ty * term
  | J of ty * (name * name * name * ty) * (name * term) * term * term * term
  | Refl of ty * term
  | Coerce of universe * universe * term
  | NameProd of universe * universe * name * term * term
  | NamePaths of universe * term * term * term
  | NameId of universe * term * term * term

let rec of_ty' k l ((t', loc) as t) =
  match t' with

    | Syntax.Universe _ ->
      Ty t

    | Syntax.El (alpha, e) ->
      let e = of_term' k l e in
      begin match e with
        | Term e -> Ty (Syntax.mkEl ~loc alpha e)
        | _ -> El (alpha, e)
      end

    | Syntax.Unit ->
      Ty t

    | Syntax.Prod (x, t1, t2) ->
      let t1 = of_ty' k l t1
      and t2 = of_ty' k (l+1) t2
      in
        begin match t1, t2 with
          | Ty t1, Ty t2 -> Ty (Syntax.mkProd ~loc x t1 t2)
          | _ -> Prod (x, t1, t2)
        end

    | Syntax.Paths (t, e1, e2) ->
      let t = of_ty' k l t
      and e1 = of_term' k l e1
      and e2 = of_term' k l e2
      in
        begin match t, e1, e2 with
          | Ty t, Term e1, Term e2 -> Ty (Syntax.mkPaths ~loc t e1 e2)
          | _ -> Paths (t, e1, e2)
        end

    | Syntax.Id (t, e1, e2) ->
      let t = of_ty' k l t
      and e1 = of_term' k l e1
      and e2 = of_term' k l e2
      in
        begin match t, e1, e2 with
          | Ty t, Term e1, Term e2 -> Ty (Syntax.mkId ~loc t e1 e2)
          | _ -> Id (t, e1, e2)
        end

and of_term' k l ((e', loc) as e) =
  match e' with

    | Syntax.Var j ->
      if j < l then Term (Syntax.mkVar ~loc j) (* bound variable *)
      else if j < k + l then PVar (j - l) (* pattern variable *)
      else Term (Syntax.mkVar ~loc (j - k)) (* other variable *)

    | Syntax.Equation (_, _, e2) ->
      of_term' k l e

    | Syntax.Rewrite (_, _, e) ->
      of_term' k l e

    | Syntax.Ascribe (e, _) ->
      of_term' k l e

    | Syntax.Lambda (x, t1, t2, e) ->
      let t1 = of_ty' k l t1
      and t2 = of_ty' k (l+1) t2
      and e = of_term' k l e
      in
        begin match t1, t2, e with
          | Ty t1, Ty t2, Term e -> Term (Syntax.mkLambda ~loc x t1 t2 e)
          | _ -> Lambda (x, t1, t2, e)
        end

    | Syntax.App (Some (x, t1, t2), e1, e2) ->
      let e1 = of_term' k l  e1
      and e2 = of_term' k l e2
      and t2 = of_ty' k (l+1) t2
      and t1 = of_ty' k l t1
      in
        begin match t1, t2, e1, e2 with
          | Ty t1, Ty t2, Term e1, Term e2 ->
            Term (Syntax.mkApp ~loc x t1 t2 e1 e2)
          | _ -> App ((x, t1, t2), e1, e2)
        end

    | Syntax.App _ ->
        Error.typing "Pattern.of_term' got an unannotated application"

    | Syntax.UnitTerm ->
      Term e

    | Syntax.Idpath (t, e) ->
      let t = of_ty' k l t
      and e = of_term' k l e
      in
        begin match t, e with
          | Ty t, Term e -> Term (Syntax.mkIdpath ~loc t e)
          | _ -> Idpath (t, e)
        end

    | Syntax.J (t, (x,y,p,u), (z,e1), e2, e3, e4) ->
      let t = of_ty' k l t
      and u = of_ty' k (l+3) u
      and e1 = of_term' k (l+1) e1
      and e2 = of_term' k l e2
      and e3 = of_term' k l e3
      and e4 = of_term' k l e4
      in
        begin match t, u, e1, e2, e3, e4 with
          | Ty t, Ty u, Term e1, Term e2, Term e3, Term e4 ->
            Term (Syntax.mkJ ~loc t (x,y,p,u) (z,e1) e2 e3 e4)
          | _ -> J (t, (x,y,p,u), (z,e1), e2, e3, e4)
        end

    | Syntax.Refl (t, e) ->
      let t = of_ty' k l t
      and e = of_term' k l e
      in
        begin match t, e with
          | Ty t, Term e -> Term (Syntax.mkRefl ~loc t e)
          | _ -> Idpath (t, e)
        end

    | Syntax.Coerce (alpha, beta, e) ->
      let e = of_term' k l e
      in
        begin match e with
          | Term e -> Term (Syntax.mkCoerce ~loc alpha beta e)
          | _ -> Coerce (alpha, beta, e)
        end

    | Syntax.NameUnit ->
      Term e

    | Syntax.NameProd (alpha, beta, x, e1, e2) ->
      let e1 = of_term' k l e1
      and e2 = of_term' k (l+1) e2
      in
        begin match e1, e2 with
          | Term e1, Term e2 -> Term (Syntax.mkNameProd ~loc alpha beta x e1 e2)
          | _ -> NameProd (alpha, beta, x, e1, e2)
        end

    | Syntax.NameUniverse _ ->
      Term e

    | Syntax.NamePaths (alpha, e1, e2, e3) ->
      let e1 = of_term' k l e1
      and e2 = of_term' k l e2
      and e3 = of_term' k l e3
      in
        begin match e1, e2, e3 with
          | Term e1, Term e2, Term e3 -> Term (Syntax.mkNamePaths ~loc alpha e1 e2 e3)
          | _ -> NamePaths (alpha, e1, e2, e3)
        end

    | Syntax.NameId (alpha, e1, e2, e3) ->
      let e1 = of_term' k l e1
      and e2 = of_term' k l e2
      and e3 = of_term' k l e3
      in
        begin match e1, e2, e3 with
          | Term e1, Term e2, Term e3 -> Term (Syntax.mkNameId ~loc alpha e1 e2 e3)
          | _ -> NameId (alpha, e1, e2, e3)
        end

let of_term k e = of_term' k 0 e

let of_ty k t = of_ty' k 0 t

(** Substitute terms for pattern variables in the given pattern, coalescing resulting
    types and terms that are free of pattern variables. *)
let rec subst_ty inst k = function

  | Ty _ as t -> t

  | El (alpha, e) ->
    let e = subst_term inst k e in
      begin match e with
        | Term e -> Ty (Syntax.mkEl alpha e)
        | _ -> El (alpha, e)
      end

  | Prod (x, t1, t2) ->
    let t1 = subst_ty inst k t1
    and t2 = subst_ty inst (k+1) t2
    in
      begin match t1, t2 with
        | Ty t1, Ty t2 -> Ty (Syntax.mkProd x t1 t2)
        | _ -> Prod (x, t1, t2)
      end

  | Paths (t, e1, e2) ->
    let t = subst_ty inst k t
    and e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    in
      begin match t, e1, e2 with
        | Ty t, Term e1, Term e2 -> Ty (Syntax.mkPaths t e1 e2)
        | _ -> Paths (t, e1, e2)
      end

  | Id (t, e1, e2) ->
    let t = subst_ty inst k t
    and e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    in
      begin match t, e1, e2 with
        | Ty t, Term e1, Term e2 -> Ty (Syntax.mkId t e1 e2)
        | _ -> Id (t, e1, e2)
      end

and subst_term inst k = function
  | Term _ as pe -> pe

  | (PVar i) as pe ->
    begin try
      let e = List.assoc i inst in
      let e = Syntax.shift k e in
        Term e
      with
        | Not_found -> pe
    end

  | Lambda (x, t1, t2, e) ->
    let t1 = subst_ty inst k t1
    and t2 = subst_ty inst (k+1) t2
    and e = subst_term inst (k+1) e
    in
      begin match t1, t2, e with
        | Ty t1, Ty t2, Term e -> Term (Syntax.mkLambda x t1 t2 e)
        | _ -> Lambda (x, t1, t2, e)
      end

  | App ((x, t1, t2), e1, e2) ->
    let t1 = subst_ty inst k t1
    and t2 = subst_ty inst (k+1) t2
    and e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    in
      begin match t1, t2, e1, e2 with
        | Ty t1, Ty t2, Term e1, Term e2 ->
          Term (Syntax.mkApp x t1 t2 e1 e2)
        | _ -> App ((x, t1, t2), e1, e2)
      end

  | Idpath (t, e) ->
    let t = subst_ty inst k t
    and e = subst_term inst k e
    in
      begin match t, e with
        | Ty t, Term e -> Term (Syntax.mkIdpath t e)
        | _ -> Idpath (t, e)
      end

  | J (t, (x, y, p, u), (z, e1), e2, e3, e4) ->
    let t = subst_ty inst k t
    and u = subst_ty inst (k+3) u
    and e1 = subst_term inst (k+1) e1
    and e2 = subst_term inst k e2
    and e3 = subst_term inst k e3
    and e4 = subst_term inst k e4
    in
      begin match t, u, e1, e2, e3, e4 with
        | Ty t, Ty u, Term e1, Term e2, Term e3, Term e4 ->
          Term (Syntax.mkJ t (x, y, p, u) (z, e1) e2 e3 e4)
        | _ -> J (t, (x, y, p, u), (z, e1), e2, e3, e4)
      end

  | Refl (t, e) ->
    let t = subst_ty inst k t
    and e = subst_term inst k e
    in
      begin match t, e with
        | Ty t, Term e -> Term (Syntax.mkRefl t e)
        | _ -> Refl (t, e)
      end

  | Coerce (alpha, beta, e) ->
    let e = subst_term inst k e
    in
      begin match e with
        | Term e -> Term (Syntax.mkCoerce alpha beta e)
        | _ -> Coerce (alpha, beta, e)
      end

  | NameProd (alpha, beta, x, e1, e2) ->
    let e1 = subst_term inst k e1
    and e2 = subst_term inst (k+1) e2
    in
      begin match e1, e2 with
        | Term e1, Term e2 ->
          Term (Syntax.mkNameProd alpha beta x e1 e2)
        | _ -> NameProd (alpha, beta, x, e1, e2)
      end

  | NamePaths (alpha, e1, e2, e3) ->
    let e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    and e3 = subst_term inst k e3
    in
      begin match e1, e2, e3 with
        | Term e1, Term e2, Term e3 ->
          Term (Syntax.mkNamePaths alpha e1 e2 e3)
        | _ -> NamePaths (alpha, e1, e2, e3)
      end

  | NameId (alpha, e1, e2, e3) ->
    let e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    and e3 = subst_term inst k e3
    in
      begin match e1, e2, e3 with
        | Term e1, Term e2, Term e3 ->
          Term (Syntax.mkNameId alpha e1 e2 e3)
        | _ -> NameId (alpha, e1, e2, e3)
      end

let rec shift_ty k l = function

  | Ty t -> Ty (Syntax.shift_ty ~bound:l k t)

  | El (alpha, e) ->
    let e = shift k l e in
      El (alpha, e)

  | Prod (x, t1, t2) ->
    let t1 = shift_ty k l t1
    and t2 = shift_ty k (l+1) t2
    in
      Prod (x, t1, t2)

  | Paths (t, e1, e2) ->
    let t = shift_ty k l t
    and e1 = shift k l e1
    and e2 = shift k l e2
    in
      Paths (t, e1, e2)

  | Id (t, e1, e2) ->
    let t = shift_ty k l t
    and e1 = shift k l e1
    and e2 = shift k l e2
    in
      Id (t, e1, e2)

and shift k l = function

  | Term e ->
    let e = Syntax.shift ~bound:l k e
    in
      Term e

  | PVar i ->
    PVar i

  | Lambda (x, t1, t2, e) ->
    let t1 = shift_ty k l t1
    and t2 = shift_ty k (l+1) t2
    and e = shift k l e
    in
      Lambda (x, t1, t2, e)

  | App ((x, t1, t2), e1, e2) ->
    let t1 = shift_ty k l t1
    and t2 = shift_ty k (l+1) t2
    and e1 = shift k l e1
    and e2 = shift k l e2
    in
      App ((x, t1, t2), e1, e2)

  | Idpath (t, e) ->
    let t = shift_ty k l t
    and e = shift k l e
    in
      Idpath (t, e)

  | J (t, (x, y, p, u), (z, e1), e2, e3, e4) ->
    let t = shift_ty k l t
    and u = shift_ty k (l+3) u
    and e1 = shift k (l+1) e1
    and e2 = shift k l e2
    and e3 = shift k l e3
    and e4 = shift k l e4
    in
      J (t, (x, y, p, u), (z, e1), e2, e3, e4)

  | Refl (t, e) ->
    let t = shift_ty k l t
    and e = shift k l e
    in
      Refl (t, e)

  | Coerce (alpha, beta, e) ->
    let e = shift k l e
    in
      Coerce (alpha, beta, e)

  | NameProd (alpha, beta, x, e1, e2) ->
    let e1 = shift k l e1
    and e2 = shift k (l+1) e2
    in
      NameProd (alpha, beta, x, e1, e2)

  | NamePaths (alpha, e1, e2, e3) ->
    let e1 = shift k l e1
    and e2 = shift k l e2
    and e3 = shift k l e3
    in
      NamePaths (alpha, e1, e2, e3)

  | NameId (alpha, e1, e2, e3) ->
    let e1 = shift k l e1
    and e2 = shift k l e2
    and e3 = shift k l e3
    in
      NameId (alpha, e1, e2, e3)

