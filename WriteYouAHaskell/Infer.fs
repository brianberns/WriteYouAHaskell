namespace WriteYou

module Infer =

    let liftA2 f xs ys =
        Seq.allPairs xs ys
            |> Seq.map (fun (x, y) -> f x y)

    let replicateM n xs =

        let rec loop n =
            if n <= 0 then
                Seq.singleton []
            else
                liftA2
                    (fun x xs -> x :: xs)
                    xs
                    (loop (n - 1))

        loop n

    let letters =
        Seq.initInfinite (fun i ->
            replicateM (i + 1) ['a'..'z']
                |> Seq.map (List.toArray >> System.String))
            |> Seq.concat
