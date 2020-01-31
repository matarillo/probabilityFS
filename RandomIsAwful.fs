module RandomIsAwful

open System
open System.Threading

let shared = Random ()
let mutable s = ""

let doIt () =
    printfn "Episode 1: Random is awful"
    printfn "In earlier days this would print 100 of the same number."
    printfn "That bug has been fixed, but you'll note we still get no sixes."

    for i = 1 to 100 do
        let random = Random ()
        printf "%d " (random.Next (1, 6))
    printfn ""

    printfn "Similarly, in earlier days this would eventually print all zeros;"
    printfn "Random is not thread safe, and its common failure mode"
    printfn "is to get into a state where it can only produce zero."
    printfn "This bug has also been fixed, though this is still a bad idea"

    for i = 1 to 100 do
        let a () = s <- (sprintf "%s%d " s (shared.Next (1, 6)))
        (Thread (ThreadStart a)).Start ()
    printfn "%s" s

    printfn "The real problem though is that this interface is not strong enough"
    printfn "to do all the interesting stuff we want to do with stochastic programming"
    printfn "in the modern era. That's what we'll be exploring in this series."
