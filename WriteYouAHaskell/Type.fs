namespace WriteYou

type TVar = TV of string

type Type =

    /// Type variable. E.g. a.
    | TVar of TVar

    /// Type constant. E.g. Int.
    | TCon of string

    /// Type arrow. E.g. 'a -> Int.
    | TArr of Type * Type

module Type =

    let int = TCon "Int"
    let bool = TCon "Bool"

/// E.g. Forall [a] (a -> a).
type Scheme = Forall of List<TVar> * Type

type TypeEnv = TypeEnv of Map<Var, Scheme>

module TypeEnv =

    let empty = TypeEnv Map.empty

    let extend x s (TypeEnv env) =
        TypeEnv (Map.add x s env)
