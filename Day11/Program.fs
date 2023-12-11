open System.IO

let mutable expansion = 1

let insertAt index newEl input =
  input |> List.mapi (fun i el -> if i = index then ((List.replicate expansion newEl) @ [el]) else [el])
        |> List.concat

let Expand(sky:char list list):char list list = 
    let mutable res = []

    for i=0 to (List.length sky)-1 do
        res <- res @ [sky.[i]]
        if sky.[i] |> Seq.forall (fun x -> x = '.') then
            res <- res @ (List.replicate expansion sky.[i])

    for j=(List.length (List.head sky))-1 downto 1 do
        if sky |> Seq.forall (fun x -> x.[j] = '.') then
            res <- res |> List.map(fun x -> insertAt j '.' x)

    res

type Galaxy = {
    Id: int
    Location: int64*int64
} 

let MapGalaxies(sky: char list list):Galaxy list = 
    let mutable res = []
    let mutable count = 1
    for i=0 to (List.length sky)-1 do
        for j=0 to (List.length (List.head sky))-1 do
            if sky.[i].[j] = '#' then
                res <- res @ [{ Id = count; Location = (i,j) }]
                count <- count + 1
    res

let ManhattanDistance(a:int64*int64) (b:int64*int64):int64 = 
    abs((fst a) - (fst b)) + abs((snd a) - (snd b))

let rec Pairs lst =
    match lst with
    | [] -> []
    | h::t -> List.map (fun elem -> (h, elem)) t @ Pairs t


let sky = 
    File.ReadLines("input.txt") 
    |> Seq.map (fun x -> x.ToCharArray() |> Array.toList) 
    |> Seq.toList

let startLocs = sky |> MapGalaxies
let expandedLocs = sky |> Expand |> MapGalaxies

let origDist =
    startLocs
        |> Pairs
        |> Seq.map(fun x -> ManhattanDistance ((fst x).Location) ((snd x).Location))
        |> Seq.sum

let expandedDist = 
    expandedLocs
        |> Pairs
        |> Seq.map(fun x -> ManhattanDistance ((fst x).Location) ((snd x).Location))
        |> Seq.sum

printfn "%s %i" "original: " origDist
printfn "%s %i" "expanded: " expandedDist
let expansionFactor = 999999L
printfn "%s %i" "final: " (((expandedDist - origDist) * expansionFactor) + origDist)