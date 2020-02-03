module Distribution

type Distribution<'T> (sample) =
    member _.Sample : unit -> 'T = sample

type DiscreteDistribution<'T> (sample, support, weight) =
    member _.Sample : unit -> 'T = sample
    member _.Support : unit -> 'T list = support
    member _.Weight : 'T -> int = weight

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

let samples<'T> (d : unit -> 'T) =
    seq {
        while true do
            yield d ()
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

let discreteHistogram<'T when 'T : equality and 'T : comparison> (d : 'T seq) =
    let sampleCount = 100000
    let width = 40
    let groups = d |> Seq.take sampleCount |> Seq.groupBy id
    let labelMax = groups |> Seq.map (fun (k, _) -> k.ToString().Length) |> Seq.max
    let toLabel f = f.ToString().PadLeft(labelMax)
    let dict = groups |> Seq.map (fun (k, v) -> k, Seq.length v) |> Map.ofSeq
    let scale =
        let max = groups |> Seq.map (fun (_, v) -> Seq.length v) |> Seq.max
        if max < width then 1.0 else (float width) / (float max)
    let bar f = System.String('*', int ((float dict.[f]) * scale))
    let sup = groups |> Seq.map fst |> Seq.sortBy toLabel
    let lines = sup |> Seq.map (fun f -> sprintf "%s|%s" (toLabel f) (bar f))
    System.String.Join(System.Environment.NewLine, lines)

let showWeights (d : DiscreteDistribution<'T>) =
    let support = d.Support ()
    let labelMax = support |> Seq.map (fun f -> f.ToString().Length) |> Seq.max
    let toLabel f = f.ToString().PadLeft(labelMax)
    let lines = support |> Seq.map (fun f -> sprintf "%s:%d" (toLabel f) (d.Weight f))
    System.String.Join(System.Environment.NewLine, lines)

// Standard Discrete Uniform
let sdu min max =
    if (min > max) then System.ArgumentException() |> raise
    let support () = [ min .. max ]
    let weight i = if min <= i && i <= max then 1 else 0
    let sample () = int (scu.Sample() * float (1 + max - min)) + min
    DiscreteDistribution<_>(sample, support, weight)
