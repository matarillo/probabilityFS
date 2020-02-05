module Episode09

open Distribution

let doIt () =
    printfn "%s" (WeightedInteger.distribution [10; 0; 0; 11; 5] |> samples |> discreteHistogram)
