(** A toplevel computation carries around the current
    environment. *)
type state = {
  desugar : Desugar.Ctx.t ;
  typing : Mlty.Ctx.t ;
  runtime : unit Runtime.progress
}

let comp_value c =
  let r = Eval.infer c in
  Runtime.top_handle ~loc:c.Location.loc r

let comp_handle (xs,y,c) =
  Runtime.top_return_closure (fun (vs,checking) ->
      let rec fold2 xs vs = match xs,vs with
        | [], [] ->
           begin match y with
           | Some y ->
              let checking = match checking with
                | Some jt -> Some (Runtime.mk_term (Jdg.term_of_ty jt))
                | None -> None
              in
              let vy = Predefined.from_option checking in
              Runtime.add_bound y vy (Eval.infer c)
           | None -> Eval.infer c
           end
        | x::xs, v::vs -> Runtime.add_bound x v (fold2 xs vs)
        | [],_::_ | _::_,[] -> Error.impossible ~loc:(c.Location.loc) "bad top handler case"
      in
      fold2 xs vs)

let comp_signature ~loc lxcs =
  let (>>=) = Runtime.bind in
  let rec fold ys yts lxts = function
    | [] ->
       let lxts = List.rev lxts in
       Runtime.return lxts

    | (l,x,c) :: lxcs ->
       Eval.check_ty c >>= fun (Jdg.Ty (ctxt,t)) ->
       if not (Context.is_subset ctxt yts)
       then Error.runtime ~loc "signature field %t has unresolved assumptions"
           (Name.print_ident l)
       else begin
         let jt = Jdg.mk_ty ctxt t
         and tabs = Tt.abstract_ty ys t in
         Runtime.add_abstracting ~loc x jt (fun _ y ->
             fold (y::ys) ((y,t)::yts) ((l,x,tabs) :: lxts) lxcs)
       end
  in
  Runtime.top_handle ~loc (fold [] [] [] lxcs)


(** Evaluation of toplevel computations *)


(** The help text printed when [#help] is used. *)
let help_text = "Toplevel directives:
#environment. .... print current environment
#help. ........... print this help
#quit. ........... exit

Parameter <ident> ... <ident> : <type> .     assume variable <ident> has type <type>
Let <ident> := <expr> .                      define <ident> to be <expr>
Check <expr> .                               check the type of <expr>

The syntax is vaguely Coq-like. The strict equality is written with a double ==.
" ;;


let (>>=) = Runtime.top_bind
let return = Runtime.top_return

let rec mfold f acc = function
  | [] -> return acc
  | x::rem -> f acc x >>= fun acc ->
     mfold f acc rem

let toplet_bind ~loc interactive xcs =
  let rec fold xvs = function
    | [] ->
       (* parallel let: only bind at the end *)
       List.fold_left
         (fun cmd (x,v) ->
            Runtime.add_topbound ~loc x v >>= fun () ->
            if interactive && not (Name.is_anonymous x)
            then Format.printf "%t is defined.@." (Name.print_ident x) ;
            cmd)
         (return ())
         xvs
    | (x, c) :: xcs ->
       comp_value c >>= fun v ->
       fold ((x, v) :: xvs) xcs
  in
  fold [] xcs

let topletrec_bind ~loc interactive fxcs =
  let gs =
    List.map
      (fun (f, x, c) -> (f, (fun v -> Runtime.add_bound x v (Eval.infer c))))
      fxcs
  in
  Runtime.add_topbound_rec ~loc gs >>= fun () ->
  if interactive then
    List.iter (fun (f, _, _) ->
        if not (Name.is_anonymous f) then
          Format.printf "%t is defined.@." (Name.print_ident f)) fxcs ;
  return ()

let rec chain_cmd ~interactive {Location.thing=c'; loc} =
  match c' with

  | Syntax.DefMLType _ -> return (Print.warning "TODO")

  | Syntax.DefMLTypeRec _ -> return (Print.warning "TODO")

  | Syntax.DeclOperation (x, k) ->
     Runtime.add_operation ~loc x >>= fun () ->
     if interactive then Format.printf "Operation %t is declared.@." (Name.print_ident x) ;
     return ()

  | Syntax.DeclConstants (xs, c) ->
     Runtime.top_handle ~loc:(c.Location.loc) (Eval.check_ty c) >>= fun (Jdg.Ty (ctxt, t)) ->
     if Context.is_empty ctxt
     then
       let rec fold = function
         | [] -> return ()
         | x :: xs ->
            Runtime.add_constant ~loc x t >>= fun () ->
            (if interactive then Format.printf "Constant %t is declared.@." (Name.print_ident x) ;
             fold xs)
       in
       fold xs
     else
       Error.typing "Constants may not depend on free variables" ~loc:(c.Location.loc)

  | Syntax.DeclSignature (s, lxcs) ->
     comp_signature ~loc lxcs >>= fun lxts ->
     Runtime.add_signature ~loc s lxts  >>= fun () ->
     (if interactive then Format.printf "Signature %t is declared.@." (Name.print_ident s) ;
      return ())

  | Syntax.TopHandle lst ->
     mfold (fun () (op, xc) ->
         comp_handle xc >>= fun f ->
         Runtime.add_handle op f) () lst

  | Syntax.TopLet xcs ->
     toplet_bind ~loc interactive xcs

  | Syntax.TopLetRec fxcs ->
     topletrec_bind ~loc interactive fxcs

  | Syntax.TopDynamic (x,c) ->
     comp_value c >>= fun v ->
     Runtime.add_dynamic ~loc x v

  | Syntax.TopNow (x,c) ->
     comp_value c >>= fun v ->
     Runtime.top_now ~loc x v

  | Syntax.TopDo c ->
     comp_value c >>= fun v ->
     Runtime.top_print_value >>= fun print_value ->
     (if interactive then Format.printf "%t@." (print_value v) ;
      return ())

  | Syntax.TopFail c ->
     Runtime.catch (fun () -> comp_value (Lazy.force c)) >>= begin function
     | Error.Err err ->
        (if interactive then Format.printf "The command failed with error:\n%t@." (Error.print err));
        return ()
     | Error.OK v ->
        Runtime.top_print_value >>= fun pval ->
        Error.runtime ~loc "The command has not failed: got %t." (pval v)
     end

  | Syntax.Included (fn, cmds) ->

     return () >>= fun () ->
     if interactive then Format.printf "#including %s@." fn ;
     return () >>= fun () ->

     (mfold (fun () c ->
         (* don't print deeper includes *)
         chain_cmd ~interactive:false c) () cmds) >>= (fun () ->
         if interactive then Format.printf "#processed %s@." fn ;
         return ())

  | Syntax.Verbosity i -> Config.verbosity := i; return ()

  | Syntax.Environment ->
     Runtime.print_env >>= fun p ->
     Format.printf "%t@." p;
     return ()

  | Syntax.Help ->
     Format.printf "%s@." help_text ; return ()

  | Syntax.Quit ->
     exit 0

let use_file ~fn ~interactive state =
  if Desugar.Ctx.included fn state.desugar then state else
    begin
      let cmds = Ulexbuf.parse Lexer.read_file Parser.file fn in
      let base_dir = Filename.dirname fn in

      let state, cmds =
        List.fold_left
          (fun (state, cmds) c ->
             match Desugar.toplevel ~base_dir state.desugar c with
             | None -> (state, cmds)
             | Some (desugar, cmd) ->
                let state = { state with desugar } in
                state, cmds @ [cmd])
          (state, []) cmds in
      let state =
        List.fold_left
          (fun state c ->
             let typing = Mlty.infer state.typing c in
             { state with typing = typing })
          state cmds in
      let cmds = mfold (fun () c -> chain_cmd ~interactive c) () cmds in
      let runtime = Runtime.step state.runtime (fun () -> cmds) in
      { state with runtime }
    end

let exec_cmd base_dir interactive cmd state =
  match Desugar.toplevel ~base_dir state.desugar cmd with
  | None -> state
  | Some (desugar, cmd) ->
     let state = { state with desugar } in
     let typing = Mlty.infer state.typing cmd in
     let state = { state with typing = typing } in
     let runtime =
       Runtime.step state.runtime
         (fun () -> chain_cmd ~interactive cmd) in
     { state with runtime }


let initial () =
  let desugar = Desugar.Ctx.empty
  and typing  = Mlty.Ctx.empty
  and runtime = Runtime.start (Runtime.top_return ()) in
  let state = { desugar; typing; runtime } in

  let typs = Ulexbuf.parse Lexer.read_string Parser.file Predefined.predefined_ml_types in
  let state =
    List.fold_left (fun state cmd ->
        (* TODO change interactive to [false] *)
        exec_cmd Filename.current_dir_name true cmd state)
      state
      typs in

  let ops = Ulexbuf.parse Lexer.read_string Parser.file Predefined.predefined_ops in
  List.fold_left (fun state cmd ->
      (* TODO change interactive to [false] *)
      exec_cmd Filename.current_dir_name true cmd state)
    state
    ops
