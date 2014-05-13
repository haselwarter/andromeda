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
        | Term e -> Ty (Syntax.El (alpha, e), loc)
        | _ -> El (alpha, e)
      end

    | Syntax.Unit ->
      Ty t

    | Syntax.Prod (x, t1, t2) ->
      let t1 = of_ty' k l t1
      and t2 = of_ty' k (l+1) t2
      in
        begin match t1, t2 with
          | Ty t1, Ty t2 -> Ty (Syntax.Prod (x, t1, t2), loc)
          | _ -> Prod (x, t1, t2)
        end

    | Syntax.Paths (t, e1, e2) ->
      let t = of_ty' k l t
      and e1 = of_term' k l e1
      and e2 = of_term' k l e2
      in
        begin match t, e1, e2 with
          | Ty t, Term e1, Term e2 -> Ty (Syntax.Paths (t, e1, e2), loc)
          | _ -> Paths (t, e1, e2)
        end

    | Syntax.Id (t, e1, e2) ->
      let t = of_ty' k l t
      and e1 = of_term' k l e1
      and e2 = of_term' k l e2
      in
        begin match t, e1, e2 with
          | Ty t, Term e1, Term e2 -> Ty (Syntax.Id (t, e1, e2), loc)
          | _ -> Id (t, e1, e2)
        end

and of_term' k l ((e', loc) as e) =
  match e' with

    | Syntax.Var j ->
      if j < l then Term (Syntax.Var j, loc) (* bound variable *)
      else if j < k + l then PVar (j - l) (* pattern variable *)
      else Term (Syntax.Var (j - k), loc) (* other variable *)

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
          | Ty t1, Ty t2, Term e -> Term (Syntax.Lambda (x, t1, t2, e), loc)
          | _ -> Lambda (x, t1, t2, e)
        end

    | Syntax.App ((x, t1, t2), e1, e2) ->
      let e1 = of_term' k l  e1
      and e2 = of_term' k l e2
      and t2 = of_ty' k (l+1) t2
      and t1 = of_ty' k l t1
      in
        begin match t1, t2, e1, e2 with
          | Ty t1, Ty t2, Term e1, Term e2 ->
            Term (Syntax.App ((x, t1, t2), e1, e2), loc)
          | _ -> App ((x, t1, t2), e1, e2)
        end

    | Syntax.UnitTerm ->
      Term e

    | Syntax.Idpath (t, e) ->
      let t = of_ty' k l t
      and e = of_term' k l e
      in
        begin match t, e with
          | Ty t, Term e -> Term (Syntax.Idpath (t, e), loc)
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
            Term (Syntax.J (t, (x,y,p,u), (z,e1), e2, e3, e4), loc)
          | _ -> J (t, (x,y,p,u), (z,e1), e2, e3, e4)
        end

    | Syntax.Refl (t, e) ->          
      let t = of_ty' k l t
      and e = of_term' k l e
      in
        begin match t, e with
          | Ty t, Term e -> Term (Syntax.Idpath (t, e), loc)
          | _ -> Idpath (t, e)
        end

    | Syntax.Coerce (alpha, beta, e) ->
      let e = of_term' k l e
      in
        begin match e with
          | Term e -> Term (Syntax.Coerce (alpha, beta, e), loc)
          | _ -> Coerce (alpha, beta, e)
        end

    | Syntax.NameUnit ->
      Term e

    | Syntax.NameProd (alpha, beta, x, e1, e2) ->
      let e1 = of_term' k l e1
      and e2 = of_term' k (l+1) e2
      in
        begin match e1, e2 with
          | Term e1, Term e2 -> Term (Syntax.NameProd (alpha, beta, x, e1, e2), loc)
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
          | Term e1, Term e2, Term e3 -> Term (Syntax.NamePaths (alpha, e1, e2, e3), loc)
          | _ -> NamePaths (alpha, e1, e2, e3)
        end

    | Syntax.NameId (alpha, e1, e2, e3) ->
      let e1 = of_term' k l e1
      and e2 = of_term' k l e2
      and e3 = of_term' k l e3
      in
        begin match e1, e2, e3 with
          | Term e1, Term e2, Term e3 -> Term (Syntax.NameId (alpha, e1, e2, e3), loc)
          | _ -> NameId (alpha, e1, e2, e3)
        end

let of_term k e = of_term' k 0 e

let of_ty k t = of_ty' k 0 t

(** Substitute terms for pattern variables in the given pattern. *)
let rec subst_ty inst k = function

  | Ty t -> t
        
  | El (alpha, e) ->
    let e = subst_term inst k e in
      Syntax.El (alpha, e), Position.nowhere
            
  | Prod (x, t1, t2) ->
    let t1 = subst_ty inst k t1
    and t2 = subst_ty inst (k+1) t2
    in
      Syntax.Prod (x, t1, t2),
      Position.nowhere
          
  | Paths (t, e1, e2) ->
    let t = subst_ty inst k t
    and e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    in
      Syntax.Paths (t, e1, e2),
      Position.nowhere
          
  | Id (t, e1, e2) ->
    let t = subst_ty inst k t
    and e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    in
      Syntax.Id (t, e1, e2),
      Position.nowhere

and subst_term inst k = function
  | Term e -> e
    
  | PVar i ->
    (* will raise [Not_found] if the [PVar i] is not unbound by [inst] *)
    let e = List.assoc i inst
    in
      Syntax.shift k e

  | Lambda (x, t1, t2, e) ->
    let t1 = subst_ty inst k t1
    and t2 = subst_ty inst (k+1) t2
    and e = subst_term inst (k+1) e
    in
      Syntax.Lambda (x, t1, t2, e),
      Position.nowhere

  | App ((x, t1, t2), e1, e2) ->
    let t1 = subst_ty inst k t1
    and t2 = subst_ty inst (k+1) t2
    and e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    in
      Syntax.App ((x, t1, t2), e1, e2),
      Position.nowhere

  | Idpath (t, e) ->
    let t = subst_ty inst k t
    and e = subst_term inst k e
    in
      Syntax.Idpath (t, e),
      Position.nowhere

  | J (t, (x, y, p, u), (z, e1), e2, e3, e4) ->
    let t = subst_ty inst k t
    and u = subst_ty inst (k+3) u
    and e1 = subst_term inst (k+1) e1
    and e2 = subst_term inst k e2
    and e3 = subst_term inst k e3
    and e4 = subst_term inst k e4
    in
      Syntax.J (t, (x, y, p, u), (z, e1), e2, e3, e4),
      Position.nowhere

  | Refl (t, e) ->
    let t = subst_ty inst k t
    and e = subst_term inst k e
    in
      Syntax.Refl (t, e),
      Position.nowhere

  | Coerce (alpha, beta, e) ->
    let e = subst_term inst k e
    in
      Syntax.Coerce (alpha, beta, e),
      Position.nowhere

  | NameProd (alpha, beta, x, e1, e2) ->
    let e1 = subst_term inst k e1
    and e2 = subst_term inst (k+1) e2
    in
      Syntax.NameProd (alpha, beta, x, e1, e2),
      Position.nowhere

  | NamePaths (alpha, e1, e2, e3) ->
    let e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    and e3 = subst_term inst k e3
    in
      Syntax.NamePaths (alpha, e1, e2, e3),
      Position.nowhere

  | NameId (alpha, e1, e2, e3) ->
    let e1 = subst_term inst k e1
    and e2 = subst_term inst k e2
    and e3 = subst_term inst k e3
    in
      Syntax.NameId (alpha, e1, e2, e3),
      Position.nowhere

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