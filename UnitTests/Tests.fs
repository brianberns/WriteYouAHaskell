namespace WriteYou

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.Unify() =
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
        Assert.AreEqual<_>(expected, actual)
