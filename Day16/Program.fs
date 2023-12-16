open System
open System.IO

type TileType =
    | RightUp
    | RightDown
    | SplitUpDown
    | SplitLR
    | Empty

type Direction = 
    | Up
    | Down
    | Left
    | Right

let input = File.ReadAllText("input.txt").Split(Environment.NewLine) |> Seq.map (fun x -> x.ToCharArray()) |> Seq.toArray
let categorized = Map.ofList [
    for i=0 to input.Length-1 do
        for j=0 to input[i].Length-1 do
            match input[i][j] with
                | '/' -> (i,j),RightUp
                | '\\' -> (i,j),RightDown
                | '|' -> (i,j),SplitUpDown
                | '-' -> (i,j),SplitLR
                | _ -> (i,j),Empty
]

let Move(from:int*int) (d: Direction):int*int*Direction*bool = 
    match categorized[from] with
        | Empty -> match d with
                    | Right -> fst from, snd from + 1, d, false
                    | Left -> fst from, snd from - 1, d, false
                    | Up -> fst from - 1, snd from, d, false
                    | Down -> fst from + 1, snd from, d, false
        | RightUp -> match d with
                        | Right -> fst from - 1, snd from, Up, false
                        | Left -> fst from + 1, snd from, Down, false
                        | Up -> fst from, snd from + 1, Right, false
                        | Down -> fst from, snd from - 1, Left, false
        | RightDown -> match d with
                        | Right -> fst from + 1, snd from, Down, false
                        | Left -> fst from - 1, snd from, Up, false
                        | Up -> fst from, snd from - 1, Left, false
                        | Down -> fst from, snd from + 1, Right, false
        | SplitLR -> match d with
                        | Right -> fst from, snd from + 1, d, false
                        | Left -> fst from, snd from - 1, d, false
                        | Up -> fst from, snd from + 1, Right, true
                        | Down -> fst from, snd from + 1, Right, true
        | SplitUpDown -> match d with
                            | Right -> fst from - 1, snd from, Up, true
                            | Left -> fst from - 1, snd from, Up, true
                            | Up -> fst from - 1, snd from, d, false
                            | Down -> fst from + 1, snd from, d, false

let mutable visited:Map<(int*int), Direction> = Map.empty
let rec AllMoves(start:int*int) (d:Direction) =
    let mutable curr = start
    let mutable dir = d
    while categorized.ContainsKey(curr) && not(visited.ContainsKey(curr) && visited[curr] = dir) do
        visited <- visited.Add(curr,dir)
        let x,y,di,s = Move curr dir
        match s,categorized[curr] with 
            | true, SplitLR -> AllMoves (fst curr, snd curr - 1) Left 
            | true, SplitUpDown -> AllMoves (fst curr + 1, snd curr) Down
            | _,_ -> ()
        curr <- (x,y)
        dir <- di

// part 1
AllMoves (0,0) Right
printfn "%i" visited.Count

// part 2
let topStarts = [0..input[0].Length-1] |> List.map (fun x -> (0,x), Down)
let bottomStarts = [0..input[0].Length-1] |> List.map (fun x -> (input.Length - 1, x), Up)
let leftStarts = [0..input.Length-1] |> List.map (fun x -> (x, 0), Right)
let rightStarts = [0..input.Length-1] |> List.map(fun x -> (x, input[0].Length - 1), Left)
let allStarts = topStarts @ leftStarts @ bottomStarts @ rightStarts

let mutable max = 0
for s in allStarts do
    visited <- Map.empty
    AllMoves (fst s) (snd s)
    if visited.Count > max then max <- visited.Count
    printfn "%i" max

printfn "%i" max