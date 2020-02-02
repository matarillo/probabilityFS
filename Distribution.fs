module Distribution

type Distribution<'T> (sample : unit -> 'T) =
    member this.Sample = sample

// Standard Continuous Uniform Distribution
let scu = Distribution<_> Pseudorandom.nextDouble

// Normal Distribution by Box-Muller Transform
let normal mean sigma =
    let standardSample () =
        let r = -2.0 * log (scu.Sample())
        let theta = 2.0 * System.Math.PI * scu.Sample()
        sqrt r * cos theta
    let sample () = mean + sigma * (standardSample ())
    Distribution<_> sample

let notmalStandard = normal 0.0 1.0

let samples<'T> (d : Distribution<'T>) =
    seq {
        while true do
            yield d.Sample ()
    }

let histogram (low : float) (high : float) (d : float seq) =
    let width = 40
    let height = 20
    let sampleCount = 100000
    let buckets = Array.zeroCreate<int> width
    let addToBucket buckets c =
        let len = Array.length buckets
        let bucket = int ((float len) * (c - low) / (high - low))
        if 0 <= bucket && bucket < len then
            buckets.[bucket] <- buckets.[bucket] + 1
    d |> Seq.take sampleCount |> Seq.iter (addToBucket buckets)
    let max = Array.max buckets
    let scale = if max < height then 1.0 else (float height) / (float max)
    let sb = System.Text.StringBuilder()
    for r = 0 to height - 1 do
        for b in buckets do
            if int (float b * scale) > (height - r) then '*' else ' '
            |> (sb.Append >> ignore)
        () |> (sb.AppendLine >> ignore)
    sb.ToString()
