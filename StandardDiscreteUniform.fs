module StandardDiscreteUniform

open Probability

let private scu = StandardContinuousUniform.distribution

// Standard Discrete Uniform
let distribution min max =
    if (min > max) then System.ArgumentException() |> raise
    let sample () = int (scu.Sample() * float (1 + max - min)) + min
    let support () = [ min .. max ]
    let weight i = if min <= i && i <= max then 1 else 0
    DiscreteDistribution<_>(sample, support, weight)
