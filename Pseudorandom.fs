module Pseudorandom

open System
open System.Threading

let private prng =
    new ThreadLocal<_>(Func<_>(fun () -> Random(BetterRandom.nextInt ())))

let nextInt () =
    prng.Value.Next ()
let nextDouble () =
    prng.Value.NextDouble ()
