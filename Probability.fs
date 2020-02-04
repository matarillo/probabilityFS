namespace Probability

type Distribution<'T> (sample) =
    member _.Sample : unit -> 'T = sample

type DiscreteDistribution<'T> (sample, support, weight) =
    member _.Sample : unit -> 'T = sample
    member _.Support : unit -> 'T list = support
    member _.Weight : 'T -> int = weight
