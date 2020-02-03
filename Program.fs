// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "Probability"
    // RandomIsAwful.doIt ()
    Episode04.doIt ()
    printfn "Press Enter to finish"
    Console.ReadLine () |> ignore
    0
