module Episode10

open Distribution

let doIt () =
    printfn "Episode 10"
    printfn "No threes, weighted integer"
    printfn "%A" ([0; 1; 1; 0; 1; 1; 1] |> weighted |> samples |> Seq.take 20 |> Seq.toList)

    printfn "No threes, conditioned uniform, rejection sampling"
    let noThrees = sdu 1 6 |> filter (fun x -> x <> 3)
    printfn "%s" (noThrees |> samples |> discreteHistogram)

    printfn "Ordinary sequence, filter clause closed over variable"
    let mutable filterOut = 3
    let predicate x = x <> filterOut
    let range = seq { 1; 2; 3; 4; 5; 6 } |> Seq.filter predicate
    printfn "%A" (range |> Seq.toList)
    printfn "Change the variable"
    filterOut <- 4
    printfn "%A" (range |> Seq.toList)

    printfn "Distribution, filter clause closed over variable"
    filterOut <- 3
    let d = sdu 1 6 |> filter predicate
    printfn "%A" (d |> samples |> Seq.take 10 |> Seq.toList)
    printfn "Change the variable"
    filterOut <- 4
    printfn "%A" (d |> samples |> Seq.take 10 |> Seq.toList)
    printfn "The support is now wrong!"
    printfn "%A" (d |> Probability.support)
    printfn "We will require that predicates and projections be pure functions."
