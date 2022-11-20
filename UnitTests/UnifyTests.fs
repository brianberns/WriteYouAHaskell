namespace WriteYou

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnifyTests() =

    let x = TV "X"
    let y = TV "Y"

    [<TestMethod>]
    member this.UnifySuccess1() =
        let t1 = TArr (TVar x, Type.int)    // X -> Int
        let t2 = TArr (Type.bool, TVar y)   // Y -> Int
        let expected =
            Ok (Map [
                x, Type.bool
                y, Type.int
            ] : Subst)
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member this.UnifySuccess2() =
        let t1 = TArr (TVar x, TVar x)     // X -> X
        let t2 = TArr (Type.int, TVar y)   // Int -> Y
        let expected =
            Ok (Map [
                x, Type.int
                y, Type.int
            ] : Subst)
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member this.UnifyFail() =
        let t1 = TArr (Type.int , TVar x)   // Int -> X
        let t2 = TArr (Type.bool, TVar y)   // Bool -> Y
        let expected =
            Error (UnificationFail (Type.int, Type.bool))
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member this.InfiniteType() =
        let t1 = TVar x                  // X
        let t2 = TArr (TVar x, TVar y)   // X -> Y
        let expected = Error (InfiniteType (x, t2))
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)
