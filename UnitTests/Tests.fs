namespace WriteYou

open Microsoft.VisualStudio.TestTools.UnitTesting

type Assert private () =

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't) =
        if actual <> expected then
            sprintf "\nExpected: %A.\nActual:   %A" expected actual
                |> Assert.Fail

[<TestClass>]
type TestClass () =

    let x = TV "X"
    let y = TV "Y"

    [<TestMethod>]
    member this.UnifySuccess() =
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
    member this.UnifyFail() =
        let t1 = TArr (Type.int , TVar x)   // Int -> X
        let t2 = TArr (Type.bool, TVar y)   // Bool -> Y
        let expected =
            Error (UnificationFail (Type.int, Type.bool))
        let actual = Subst.unify t1 t2
        Assert.AreEqual<_>(expected, actual)
