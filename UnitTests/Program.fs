namespace WriteYou

module Program =

    let x = TV "X"
    let y = TV "Y"
    let t1 = TArr (TVar x, Type.int)
    let t2 = TArr (Type.bool, TVar y)
    let expected =
        Ok (Map [
            x, Type.bool
            y, Type.int
        ] : Subst)
    let actual = Subst.unify t1 t2
    printfn "%A" actual
    assert(actual = expected)
