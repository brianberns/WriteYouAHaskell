namespace WriteYou

type Name = string

type Lit =
    | LInt of int
    | LBool of bool

type Binop =
    | Add | Sub | Mul | Eql

type Var = string

type Expr =
    | Var of Var
    | App of Expr * Expr
    | Lam of Name * Expr
    | Let of Name * Expr * Expr
    | Lit of Lit
    | If of Expr * Expr * Expr
    | Fix of Expr
    | Op of Binop * Expr * Expr

(*
let add x y = x + y; => let add = \x -> \y -> x + y;
*)
type Decl = string * Expr

type Program = Program of List<Decl> * Expr
