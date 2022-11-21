namespace WriteYou

open Microsoft.VisualStudio.TestTools.UnitTesting
open WriteYou

[<TestClass>]
type InferTests() =

    [<TestMethod>]
    member this.Infer() =
        let expr =
            Let (
                "x",
                Lit (LInt 2),
                Op (Mul, Lit (LInt 3), Var "x"))
        let expected =
            Ok (Subst.empty, Type.int)
                |> Result.map snd
        let actual =
            Infer.infer TypeEnv.empty expr
                |> Result.map snd
        Assert.AreEqual(expected, actual)
