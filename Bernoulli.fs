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
        let sample () =
            let p = float zero / float (zero + one)
            if scu.Sample() <= p then 0 else 1
        let support () = [0; 1]
        let weight i =
            match i with
            | 0 -> zero
            | 1 -> one
            | _ -> 0
        DiscreteDistribution<_>(sample, support, weight)
