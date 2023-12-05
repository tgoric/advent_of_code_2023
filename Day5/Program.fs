open System.IO

let file = File.ReadLines("input.txt") |> Seq.toList

let Lookup(starting: string) (ending: string) (v: int64): int64 = 
    let starts = file |> Seq.findIndex(fun x -> x.StartsWith(starting))
    let ends = file |> Seq.findIndex(fun x -> x.StartsWith(ending))
    let lines = [starts+1 .. ends-2] |> Seq.map (fun i -> file.[i])
    let values = lines |> Seq.map (fun x -> 
            x.Split(" ")
                |> Seq.map (fun x -> x |> int64)
                |> Seq.toList
        )
    let ordered = values |> Seq.sortBy (fun x -> x.[1])
    let found = ordered |> Seq.filter (fun x -> x.[1] <= v) |> Seq.rev
    match found with 
        | x when Seq.isEmpty x -> v
        | _ -> 
            let f = found |> Seq.head
            if v < (f.[1] + f.[2]) then f.[0] + (v - f.[1])
            else v
    
let FindLocation(seed: int64):int64 = 
    Lookup "seed-" "soil" seed
        |> Lookup "soil" "fertilizer"
        |> Lookup "fertilizer" "water"
        |> Lookup "water" "light"
        |> Lookup "light" "temp"
        |> Lookup "temp" "humid"
        |> Lookup "humid" "end"

// part 1
let seeds = file.[0].Split(": ").[1].Split(" ") |> Seq.map (fun x -> x |> int64)
seeds 
    |> Seq.map FindLocation
    |> Seq.min
    |> printfn "%i"