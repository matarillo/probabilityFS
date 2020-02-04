module Episode04

open Distribution

let doIt () =
    printfn "Episode 04"
    printfn "10d6:"
    printfn "%d" (sdu 1 6 |> samples |> Seq.take 10 |> Seq.sum)
    printfn "1d10:"
    printfn "%s" (sdu 1 10 |> samples |> discreteHistogram)
    printfn "1d6:"
    printfn "%s" (sdu 1 6 |> showWeights)
