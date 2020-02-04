module Singleton

open Probability

let distribution<'T when 'T : equality> (value : 'T) =
    let fSample () = value
    let fSupport () = [value]
    let fWeight t = if value = t then 1 else 0
    DiscreteDistribution<_>(fSample, fSupport, fWeight)
