open System
open System.IO

let Hash(s: string): int =
    let mutable curr = 0
    for i in s do
        curr <- ((curr + (i |> int)) * 17) % 256
    curr

// part 1
File.ReadAllText("input.txt").Split(",") 
    |> Seq.map Hash
    |> Seq.sum
    |> printfn "%i"

type Operation = 
    | Remove
    | Add

type content = {
    Label: string
    Content: string
    FocalLength: int
}
let mutable boxes:content list array = [0..255] |> List.map (fun x -> []) |> List.toArray

let input = File.ReadAllText("input.txt").Split(",")
for i in input do
    let s = i.Split([|'=';'-'|], StringSplitOptions.RemoveEmptyEntries)
    let label = s.[0]
    let mutable box = boxes.[Hash label]
    match s.Length with 
        | 1 -> box <- box |> List.filter(fun x -> not(x.Label = label))
        | _ -> 
            let idx = box |> List.tryFindIndex (fun x -> x.Label = label)
            let elem = {Label=label;Content=s.[0] + " " + s.[1];FocalLength = s.[1]|>int}
            match idx with
                | Some y -> box <- box |> List.mapi(fun i x -> if i=y then elem else x)
                | None -> box <- box @ [elem]
    boxes.[Hash label] <- box

// part 2
boxes 
    |> Array.mapi(fun i x -> 
        x 
            |> List.mapi(fun j y -> (i+1) * ((j+1) * y.FocalLength)) 
            |> List.sum
    ) 
    |> Seq.sum 
    |> printfn "%i"