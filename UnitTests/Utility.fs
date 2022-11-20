namespace WriteYou

open Microsoft.VisualStudio.TestTools.UnitTesting

type Assert private () =

    // Improves error message for F# types (e.g. discriminated unions).
    static member AreEqual<'t when 't : equality>(expected : 't, actual : 't) =
        if actual <> expected then
            sprintf "\nExpected: %A.\nActual:   %A" expected actual
                |> Assert.Fail
