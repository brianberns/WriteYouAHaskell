namespace WriteYou

type Name = string

type Lit =
    | LInt of int
    | LBool of bool

type Binop =
    | Add | Sub | Mul | Eql

type Var = V of Name

type Expr =
    | Var of Var
    | App of Expr * Expr
    | Lam of Name * Expr
    | Let of Name * Expr * Expr
    | Lit of Lit
    | If of Expr * Expr * Expr
    | Fix of Expr
    | Op of Binop * Expr * Expr

type Decl = string * Expr

type Program = Program of List<Decl> * Expr
