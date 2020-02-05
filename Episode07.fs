module Episode07

open Distribution

let doIt () =
    printfn "%s" (WeightedInteger.distribution [10; 11; 5] |> samples |> discreteHistogram)
