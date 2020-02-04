module Probability

type Distribution<'T> (sample) =
    member _.Sample() : 'T = sample ()

type DiscreteDistribution<'T> (sample, support, weight) =
    inherit Distribution<'T>(sample)
    member _.Support() : 'T list = support ()
    member _.Weight(t : 'T) : int = weight t

let sample (distribution : Distribution<_>) = distribution.Sample()

let weight (distribution : DiscreteDistribution<_>) = distribution.Weight()

let support (distribution : DiscreteDistribution<_>) = distribution.Support()
