namespace WriteYou

type TVar = TV of string

type Type =
    | TVar of TVar
    | TCon of string
    | TArr of Type * Type

module Type =

    let int = TCon "Int"
    let bool = TCon "Bool"

type Scheme = Forall of List<TVar> * Type

type TypeEnv = TypeEnv of Map<Var, Scheme>

module TypeEnv =

    let extent x s (TypeEnv env) =
        TypeEnv (Map.add x s env)
