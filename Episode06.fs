module Episode06

type Animal = Cat | Dog | Goldfish

open Distribution

let doIt () =
    let animals : Animal list = [ Cat; Dog; Dog; Goldfish ]
    printfn "%s" (animals |> toUniform |> samples |> discreteHistogram)
    printfn "%s" (animals |> toUniform |> showWeights)
