namespace WriteYou

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type UnifyTests() =

    let x = TV "X"
    let y = TV "Y"

    [<TestMethod>]
    member this.UnifySuccess1() =
        let t1 = TVar x => Type.int
        let t2 = Type.bool => TVar y
        let expected =
            Ok (Map [
                x, Type.bool
                y, Type.int
            ] : Subst)
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member this.UnifySuccess2() =
        let t1 = TVar x => TVar x
        let t2 = Type.int => TVar y
        let expected =
            Ok (Map [
                x, Type.int
                y, Type.int
            ] : Subst)
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member this.UnifyFail() =
        let t1 = Type.int => TVar x
        let t2 = Type.bool => TVar y
        let expected =
            Error (UnificationFail (Type.int, Type.bool))
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)

    [<TestMethod>]
    member this.InfiniteType() =
        let t1 = TVar x
        let t2 = TVar x => TVar y
        let expected = Error (InfiniteType (x, t2))
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)
