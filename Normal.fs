module Normal

open Probability

let private scu = StandardContinuousUniform.distribution

// Normal Distribution by Box-Muller Transform
let distribution mean sigma =
    let standardSample () =
        let r = -2.0 * log (sample scu)
        let theta = 2.0 * System.Math.PI * sample scu
        sqrt r * cos theta
    let fSample () = mean + sigma * (standardSample ())
    Distribution<_> fSample
