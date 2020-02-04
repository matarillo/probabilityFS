module Singleton

open Probability

let distribution<'T when 'T : equality> (value : 'T) =
    let sample () = value
    let support () = [value]
    let weight t = if value = t then 1 else 0
    DiscreteDistribution<_>(sample, support, weight)
