module Bernoulli

open Probability

let private scu = StandardContinuousUniform.distribution

let distribution zero one =
    if zero < 0 || one < 0 || (zero = 0 && one = 0) then System.ArgumentException() |> raise
    if zero = 0 then
        Singleton.distribution 1
    elif one = 0 then
        Singleton.distribution 0
    else
        let gcd = Extensions.gcd zero one
        let (zero, one) = (zero / gcd, one / gcd)
        let fSample () =
            let p = float zero / float (zero + one)
            if sample scu <= p then 0 else 1
        let fSupport () = [0; 1]
        let fWeight i =
            match i with
            | 0 -> zero
            | 1 -> one
            | _ -> 0
        DiscreteDistribution<_>(fSample, fSupport, fWeight)
