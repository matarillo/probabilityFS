module Normal

open Probability

let private scu = StandardContinuousUniform.distribution

// Normal Distribution by Box-Muller Transform
let distribution mean sigma =
    let standardSample () =
        let r = -2.0 * log (scu.Sample())
        let theta = 2.0 * System.Math.PI * scu.Sample()
        sqrt r * cos theta
    let sample () = mean + sigma * (standardSample ())
    Distribution<_> sample
