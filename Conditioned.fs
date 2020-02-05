module Conditioned

open Probability

let distribution (predicate : 'T -> bool) (underlying : DiscreteDistribution<'T>) =
    let filtered = underlying |> support |> List.filter predicate
    let fSample () = underlying |> samples |> Seq.find predicate
    let fSupport () = filtered
    let fWeight t =  if predicate t then underlying |> weight t else 0
    DiscreteDistribution<_>(fSample, fSupport, fWeight)
