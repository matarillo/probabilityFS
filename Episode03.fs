module Episode03

open Distribution

let doIt () =
    printfn "Episode 03"
    printfn "The sum of 12 random doubles:"
    printfn "%f" (scu |> samples |> Seq.take 12 |> Seq.sum)
    printfn "A histogram of the SCU:"
    printfn "%s" (scu |> samples |> histogram 0.0 1.0)
    printfn "A histogram of a Gaussian:"
    printfn "%s" (normal 1.0 1.5 |> samples |> histogram -4.0 4.0)
