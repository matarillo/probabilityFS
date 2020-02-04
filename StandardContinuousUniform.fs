module StandardContinuousUniform

open Probability

// Standard Continuous Uniform Distribution
let distribution =
    Distribution<_> Pseudorandom.nextDouble
