namespace WriteYou

type Subst = Map<TVar, Type>

module Subst =

    let empty : Subst = Map.empty

    module private Map =

        let map' f =
            Map.map (fun _ v -> f v)

        let union m1 m2 =
            Seq.append (Map.toSeq m2) (Map.toSeq m1)   // left-biased
                |> Map.ofSeq

    module Type =

        let rec apply (s : Subst) = function
            | TCon a -> TCon a
            | TVar a as t ->
                s
                    |> Map.tryFind a
                    |> Option.defaultValue t
            | TArr (t1, t2) ->
                TArr (apply s t1, apply s t2)

        let rec ftv = function
            | TCon _ -> Set.empty
            | TVar a -> set [a]
            | TArr (t1, t2) -> ftv t1 + ftv t2

    let compose (s1 : Subst) (s2 : Subst) : Subst =
        Map.map' (Type.apply s1) s2
            |> Map.union s1

    let (++) = compose

    let rec unify t1 t2 =

        let occursCheck a t =
            Set.contains a (Type.ftv t)

        let bind a t =
            if t = TVar a then
                Ok empty
            elif occursCheck a t then
                Error $"Infinite type {a} {t}"
            else
                Ok (Map [a, t])

        match t1, t2 with
            | TArr (l, r), TArr (l', r') ->
                result {
                    let! s1 = unify l l'
                    let! s2 = unify (Type.apply s1 r) (Type.apply s1 r')
                    return s1 ++ s2
                }
            | TVar a, t
            | t, TVar a ->
                bind a t
            | (TCon a), (TCon b) when a = b ->
                Ok empty
            | _ -> Error $"Unification fail {t1} {t2}"

    module Scheme =

        let apply (s : Subst) (Forall (vs, t)) =
            let s' : Subst = List.foldBack Map.remove vs s
            Forall (vs, Type.apply s' t)

        let ftv (Forall (vs, t)) =
            Type.ftv t - set vs

    module TypeEnv =

        let apply s (TypeEnv env) =
            TypeEnv (Map.map' (Scheme.apply s) env)

        let ftv (TypeEnv env) =
            Seq.collect Scheme.ftv (Map.values env)
                |> set
