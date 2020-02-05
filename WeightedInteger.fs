module WeightedInteger

open Probability

let private sdu = StandardDiscreteUniform.distribution
let private bern = Bernoulli.distribution

// Weighted integer distribution using rejection sampling method.
let distribution weights =
    if List.exists (fun x -> x < 0) weights then System.ArgumentException() |> raise
    if not <| List.exists (fun x -> x > 0) weights then System.ArgumentException() |> raise
    match weights with
    | [_]         -> Singleton.distribution 0
    | [zero; one] -> Bernoulli.distribution zero one
    | _           ->
        let weights = Array.ofList weights
        let max = Array.max weights
        let distributions = weights |> Array.map (fun w -> bern w (max - w))
        let fSample () =
            let rows = sdu 0 (weights.Length - 1)
            let rec loop () =
                let row = rows |> sample
                let s = distributions.[row] |> sample
                if s = 0 then row else loop ()
            loop ()
        let fSupport () = [0 .. weights.Length - 1] |> List.filter (fun i -> weights.[i] > 0)
        let fWeight i = if 0 <= i && i < weights.Length then weights.[i] else 0
        DiscreteDistribution<_>(fSample, fSupport, fWeight)
