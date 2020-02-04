module Projected

open Probability

let distribution<'A, 'R when 'R : equality and 'R : comparison> (projection : 'A -> 'R) (underlying : DiscreteDistribution<'A>) =
    let groups =
        underlying
        |> support
        |> List.groupBy projection
    let keys =
        groups
        |> List.map fst
    let weights =
        let sumWeight = List.map underlying.Weight >> List.sum
        groups
        |> List.map (fun (k, vs) -> (k, sumWeight vs))
        |> Map.ofList
    let fSample () = underlying |> sample |> projection
    let fSupport () = keys
    let fWeight t =
        match Map.tryFind t weights with
        | Some w -> w
        | _      -> 0
    DiscreteDistribution<_>(fSample, fSupport, fWeight)
