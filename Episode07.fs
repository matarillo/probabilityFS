module Episode07

open Distribution

let doIt () =
    printfn "%s" ([10; 11; 5] |> weighted |> samples |> discreteHistogram)
