open System.IO
open System.Drawing

type Element = 
    | VPipe
    | HPipe
    | NEConnector
    | NWConnector
    | SWConnector
    | SEConnector
    | Start
    | None

type Direction = 
    | North
    | East
    | South
    | West

type LoopCell = {
    Location: int*int
    Direction: Direction
    CellType: Element
}

let chars = 
    File.ReadLines("input.txt") 
        |> Seq.map (fun x -> x.ToCharArray())
        |> Seq.toArray

let characterized = 
    Map.ofSeq [for i=0 to chars.Length-1 do
                   for j=0 to chars.[0].Length-1 do
                       match chars.[i].[j] with
                           | '|' -> ((i,j), VPipe)
                           | '-' -> ((i,j), HPipe)
                           | 'L' -> ((i,j), NEConnector)
                           | 'J' -> ((i,j), NWConnector)
                           | '7' -> ((i,j), SWConnector)
                           | 'F' -> ((i,j), SEConnector)
                           | 'S' -> ((i,j), Start)
                           | '.' -> ((i,j), None)
               ]

let mutable loop: LoopCell list = []

let rec Travel(from:int*int)(dir:Direction)(distance:int):(int*int)*int = 
    let x = characterized[from]
    loop <- loop @ [{Location=from;Direction=dir;CellType=x}]
    match x with
        | VPipe -> 
            let next = if dir = North then ((fst from)-1, snd from) else ((fst from)+1, snd from)
            Travel next dir (distance + 1)
        | HPipe -> 
            let next = if dir = West then (fst from, (snd from)-1) else (fst from, (snd from)+1)
            Travel next dir (distance + 1)
        | NEConnector -> 
            match dir with
                | West -> Travel ((fst from)-1, snd from) North (distance + 1)
                | South -> Travel (fst from, (snd from)+1) East (distance + 1)
        | NWConnector -> 
            match dir with
                | East -> Travel ((fst from)-1, snd from) North (distance + 1)
                | South -> Travel (fst from, (snd from)-1) West (distance + 1)
        | SWConnector -> 
            match dir with
                | East -> Travel ((fst from)+1, snd from) South (distance + 1)
                | North -> Travel (fst from, (snd from)-1) West (distance + 1)
        | SEConnector -> 
            match dir with
                | West -> Travel ((fst from)+1, snd from) South (distance + 1)
                | North -> Travel (fst from, (snd from)+1) East (distance + 1)
        | Start when distance = 0 -> Travel ((fst from)+1, snd from) dir (distance + 1)
        | Start -> from,distance

// part 1
let start = characterized |> Seq.filter (fun x -> x.Value = Start) |> Seq.exactlyOne
let res = Travel start.Key South 0
printfn "%i" ((snd res)/2)

// part 2
let mutable count = 0
let mutable IsInLoop = false
for i=0 to chars.Length-1 do
    for j=0 to chars.[0].Length-1 do
        let element = loop |> Seq.filter (fun x -> x.Location = (i,j) && not(x.Location = start.Key))
        if not(Seq.isEmpty element) then
            let e = Seq.exactlyOne element
            let isUp = 
                (e.CellType = VPipe) ||
                (e.CellType = NEConnector) ||
                (e.CellType = NWConnector)
            if(isUp) then 
                IsInLoop <- not(IsInLoop)
        elif IsInLoop then
            count <- count + 1
            

printfn "%i" (count-1)