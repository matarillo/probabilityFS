module Episode05

open Distribution

let doIt () =
    printfn "Episode 05"
    printfn "Bernoulli 75%% chance of 1"
    printfn "%s" (bernoulli 1 3 |> samples |> discreteHistogram)
