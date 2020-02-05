module WeightedInteger

open Probability

let private sdu = StandardDiscreteUniform.distribution

let distribution weights =
    if List.exists (fun x -> x < 0) weights then System.ArgumentException() |> raise
    if not <| List.exists (fun x -> x > 0) weights then System.ArgumentException() |> raise
    match weights with
    | [_]         -> Singleton.distribution 0
    | [zero; one] -> Bernoulli.distribution zero one
    | _           ->
        // inverse transform sampling
        let weights = Array.ofList weights
        let cumulateBack acc (x : int) =
            match acc with
            | head::tail -> (x + head)::head::tail
            | []         -> [x]
        let cumulative = weights |> Array.fold cumulateBack [] |> List.rev
        let fSample () =
            let total = List.last cumulative
            let uniform = sdu 1 total |> sample
            List.findIndex (fun c -> c >= uniform) cumulative
        let fSupport () = [0 .. weights.Length - 1] |> List.filter (fun i -> weights.[i] > 0)
        let fWeight i = if 0 <= i && i < weights.Length then weights.[i] else 0
        DiscreteDistribution<_>(fSample, fSupport, fWeight)
