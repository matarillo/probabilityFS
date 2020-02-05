module Episode09

open Distribution

let doIt () =
    printfn "%s" ([10; 0; 0; 11; 5] |> weighted |> samples |> discreteHistogram)
