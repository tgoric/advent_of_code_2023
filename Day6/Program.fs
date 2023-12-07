open System
open System.IO

type Race = {
    Time: int64
    RecordDistance: int64
}

let ParseFile (s: string):Race seq = 
    let lines = s.Split(Environment.NewLine)
    let times = lines.[0].Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.skip 1 |> Seq.toList
    let distances = lines.[1].Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.skip 1 |> Seq.toList
    times 
        |> Seq.mapi(fun i x -> { Time = times.[i] |> int64; RecordDistance = distances.[i] |> int64 })

let OptimizePerformance(r: Race) = 
    [0L .. r.Time] 
        |> Seq.map (fun x -> x * (r.Time - x))
        |> Seq.filter (fun x -> x > r.RecordDistance) 
        |> Seq.length

// part 1
ParseFile (File.ReadAllText("input.txt"))
    |> Seq.map OptimizePerformance 
    |> Seq.reduce(fun acc x -> acc*x) 
    |> printfn "%i"

// part 2
ParseFile (File.ReadAllText("input2.txt"))
    |> Seq.map OptimizePerformance
    |> Seq.exactlyOne
    |> printfn "%i"