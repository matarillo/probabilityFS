module StandardDiscreteUniform

open Probability

let private scu = StandardContinuousUniform.distribution

// Standard Discrete Uniform
let distribution min max =
    if (min > max) then System.ArgumentException() |> raise
    let fSample () = int (sample scu * float (1 + max - min)) + min
    let fSupport () = [ min .. max ]
    let fWeight i = if min <= i && i <= max then 1 else 0
    DiscreteDistribution<_>(fSample, fSupport, fWeight)
