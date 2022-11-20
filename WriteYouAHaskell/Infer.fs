namespace WriteYou

module Infer =

    let liftA2 f xs ys =
        Seq.allPairs xs ys
            |> Seq.map (fun (x, y) -> f x y)

    let replicateM cnt0 f =

        let rec loop cnt =
            if cnt <= 0 then
                Seq.singleton []
            else
                liftA2
                    (fun x xs -> x :: xs)
                    f
                    (loop (cnt - 1))

        loop cnt0

    let letters =
        Seq.initInfinite (fun i ->
            replicateM (i + 1) ['a'..'z']
                |> Seq.map (List.toArray >> System.String))
            |> Seq.concat
