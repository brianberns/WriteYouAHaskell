namespace WriteYou

[<AutoOpen>]
module Operator =

    let (=>) t1 t2 = TArr (t1, t2)

module Infer =

    open Subst

    let mutable count = 0

    let fresh _ =
        count <- count + 1
        $"tv{count}" |> TV |> TVar

    let instantiate (Forall (vs, t)) =
        let vs' = List.map fresh vs
        let s = Map (List.zip vs vs')
        Type.apply s t

    let generalize env t =
        let vs =
            Set.toList (Type.ftv t - TypeEnv.ftv env)
        Forall (vs, t)

    let private lookupEnv (TypeEnv env) x =
        result {
            match Map.tryFind x env with
                | None ->
                    return! Error (UnboundVariable x)
                | Some s ->
                    let t = instantiate s
                    return Subst.empty, t
        }

    let private ops =
        Map [
            Add, Type.int => Type.int => Type.int
            Mul, Type.int => Type.int => Type.int
            Sub, Type.int => Type.int => Type.int
            Eql, Type.int => Type.int => Type.bool   // to-do: make polymorphic?
        ]

    let rec infer env ex =
        result {
            match ex with
                | Var x ->
                    return! lookupEnv env x
                | Lam (x, e) ->
                    let tv = fresh ()
                    let env' = TypeEnv.extend x (Forall ([], tv)) env
                    let! s1, t1 = infer env' e
                    return s1, Type.apply s1 tv => t1
                | App (e1, e2) ->
                    let tv = fresh ()
                    let! s1, t1 = infer env e1
                    let! s2, t2 = infer (TypeEnv.apply s1 env) e2
                    let! s3 = unify (Type.apply s2 t1) (t2 => tv)
                    return s3 ++ s2 ++ s1, Type.apply s3 tv
                | Let (x, e1, e2) ->
                    let! s1, t1 = infer env e1
                    let env' = TypeEnv.apply s1 env
                    let t' = generalize env' t1
                    let! s2, t2 = infer (TypeEnv.extend x t' env') e2
                    return s1 ++ s2, t2
                | If (cond, tr, fl) ->
                    let! s1, t1 = infer env cond
                    let! s2, t2 = infer env tr
                    let! s3, t3 = infer env fl
                    let! s4 = unify t1 Type.bool
                    let! s5 = unify t2 t3
                    return s5 ++ s4 ++ s3 ++ s2 ++ s1, Type.apply s5 t2
                | Fix e1 ->
                    let! s1, t = infer env e1
                    let tv = fresh ()
                    let! s2 = unify (tv => tv) t
                    return s2, Type.apply s1 tv
                | Op (op, e1, e2) ->
                    let! s1, t1 = infer env e1
                    let! s2, t2 = infer env e2
                    let tv = fresh ()
                    let! s3 = unify (t1 => t2 => tv) ops[op]
                    return s1 ++ s2 ++ s3, Type.apply s3 tv
                | Lit (LInt _) ->
                    return Subst.empty, Type.int
                | Lit (LBool _) ->
                    return Subst.empty, Type.bool
        }

    let private closeOver sub ty =
        generalize TypeEnv.empty (Type.apply sub ty)

    let private inferExpr env ex =
        result {
            let! sub, ty = infer env ex
            return closeOver sub ty
        }

    let rec inferTop env = function
        | [] -> Ok env
        | ((name, ex) : Decl) :: xs ->
            result {
                let! sc = inferExpr env ex
                let env' = TypeEnv.extend name sc env
                return! inferTop env' xs
            }