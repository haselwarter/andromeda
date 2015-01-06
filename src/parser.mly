%{
  open Input
%}

%token FORALL FUN
%token TYPE
%token UNDERSCORE
%token <string> NAME
%token LPAREN RPAREN
%token COLON ASCRIBE COMMA DOT
%token ARROW DARROW
%token EQEQ
%token REFL
%token TOPLET TOPCHECK
%token LET COLONEQ AND IN
%token PARAMETER
%token CONTEXT HELP QUIT
%token EOF

%start <Input.toplevel list> file
%start <Input.toplevel> commandline

%%

(* Toplevel syntax *)

file:
  | f=filecontents EOF            { f }

filecontents:
  |                                 { [] }
  | d=topcomp ds=filecontents        { d :: ds }
  | d=topdirective ds=filecontents  { d :: ds }

commandline:
  | topcomp EOF       { $1 }
  | topdirective EOF { $1 }

(* Things that can be defined on toplevel. *)
topcomp: mark_position(plain_topcomp) { $1 }
plain_topcomp:
  | TOPLET x=name COLONEQ c=term DOT                     { TopLet (x, c) }
  | TOPCHECK c=term DOT                                  { TopCheck c }
  | PARAMETER xs=nonempty_list(name) COLON t=term DOT    { Parameter (xs, t) }
    
(* Toplevel directive. *)
topdirective: mark_position(plain_topdirective) { $1 }
plain_topdirective:
  | CONTEXT    { Context }
  | HELP       { Help }
  | QUIT       { Quit }

(* Main syntax tree *)

term: mark_position(plain_term) { $1 }
plain_term:
  | e=plain_ty_term                                 { e }
  | LET a=let_clauses IN c=term                     { Let (a, c) }
  | e=app_term ASCRIBE t=ty_term                    { Ascribe (e, t) }

ty_term: mark_position(plain_ty_term) { $1 }
plain_ty_term:
  | e=plain_equal_term                              { e }
  | FORALL a=abstraction(ty_term) COMMA e=term      { Prod (a, e) }
  | FUN a=abstraction(ty_term) DARROW e=term        { Lambda (a, e) }
  | t1=equal_term ARROW t2=ty_term                  { Prod ([(Common.anonymous, t1)], t2) }

equal_term: mark_position(plain_equal_term) { $1 }
plain_equal_term:
  | e=plain_app_term                                { e }
  | e1=app_term EQEQ e2=app_term                    { Eq (e1, e2) }

app_term: mark_position(plain_app_term) { $1 }
plain_app_term:
  | e=plain_simple_term                             { e }
  | e=simple_term es=nonempty_list(simple_term)     { Spine (e, es) }
  | REFL e=simple_term                              { Refl e }

simple_term: mark_position(plain_simple_term) { $1 }
plain_simple_term:
  | TYPE                                            { Type }
  | x=name                                          { Var x }
  | LPAREN e=plain_term RPAREN                      { e }

name:
  | NAME { Common.to_name $1 }
  | UNDERSCORE { Common.anonymous }

let_clauses:
  | ls=separated_nonempty_list(AND, let_clause)     { ls }

let_clause:
  | x=name COLONEQ c=term                           { (x,c) }

(* returns a list of things individually annotated by positions.
  Since the list is not further annotated, consistency suggests
  this should be called plain_abstraction, but as we know,
  consistency is the hemoglobin of mindless lights. *)
abstraction(X):
  | bind(X)                            { $1 }
  | nonempty_list(paren_bind(X))       { List.concat $1 }

bind(X):
  | xs=nonempty_list(name) COLON t=X   { List.map (fun x -> (x, t)) xs }

paren_bind(X):
  | LPAREN b=bind(X) RPAREN            { b }

mark_position(X):
  x=X
  { x, Position.make $startpos $endpos }

%%