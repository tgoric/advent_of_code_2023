open System
open System.IO

type Cell = {
    Position: int * int
    Adjacents: (int * int) list
}

type Num = {
    Index: int
    Value: int
    Positions: (int * int) list
}

let lines = File.ReadLines("input.txt") |> Seq.toList
//let lines = Seq.toList [
//    ".501..."
//    "..*...."
//    "...609."
//]
let line_count = List.length lines

// create adjacency list giving indexes of the matrix that are adjacent to each cell
let adjacency = lines |> Seq.mapi (fun i x ->
        let line_length = Seq.length x
        x |> Seq.mapi (fun j y ->
            let a = 
                match i, j with
                    | 0, 0 -> [ (i, j+1); (i+1, j); (i+1, j+1) ]
                    | 0, z when z = line_length-1 -> [ (i, j-1); (i+1, j-1); (i+1, j) ]
                    | 0, _ -> [ (i,j-1); (i+1, j-1); (i+1, j); (i+1, j+1); (i, j+1) ]
                    | z, 0 when z = line_count-1 -> [ (i-1, j); (i-1, j+1); (i, j+1) ]
                    | z, a when z = line_count-1 && a = line_length-1 -> [ (i, j-1); (i-1, j-1); (i-1, j) ]
                    | z, _ when z = line_count-1 -> [ (i, j-1); (i-1, j-1); (i-1, j); (i-1, j+1); (i, j+1) ]
                    | _, 0 -> [ (i-1, j); (i-1, j+1); (i, j+1); (i+1, j+1); (i+1, j); ]
                    | _, z when z = line_length-1 -> [ (i, j-1); (i-1, j-1); (i-1, j); (i+1, j); (i+1, j-1) ]
                    | _, _ -> [ (i, j-1); (i-1, j-1); (i-1, j); (i-1, j+1); (i, j+1); (i+1, j+1); (i+1, j); (i+1, j-1) ]
            
            {Position = (i, j); Adjacents = a}
        )
    ) 
    
let adjacency_list = adjacency |> Seq.collect id

// create a list of the actual numbers and the positions they occupy
let mutable nums:Num list = []
let mutable num_count = 0

lines |> Seq.iteri (fun i x ->
    let mutable num_str = ""
    let mutable positions = []
    let length = x |> Seq.length
    x |> Seq.iteri (fun j y ->
        if Char.IsDigit y then
            num_str <- num_str + y.ToString()
            positions <- positions @ [(i, j)]
        if num_str <> "" && (j = (length-1) || not(Char.IsDigit(y))) then
            nums <- nums @ [{ Index = num_count; Value = num_str |> int; Positions = positions }]
            num_str <- ""
            positions <- []
            num_count <- num_count + 1
    )
)


let IsSymbol(pos:(int * int)):bool = 
    let x = lines.[(fst pos)].[(snd pos)]
    not(x = '.') && not(Char.IsDigit x)

// find the numbers that have an adjacent symbol, and sum them
let HasAdjacentSymbol(n:Num):bool = 
    n.Positions |> Seq.exists (fun p -> 
        adjacency_list 
            |> Seq.filter (fun x -> x.Position = p) 
            |> Seq.map _.Adjacents 
            |> Seq.exactlyOne
            |> Seq.exists IsSymbol
    )

// part 1
nums 
    |> Seq.filter HasAdjacentSymbol 
    |> Seq.map _.Value
    |> Seq.sum
    |> printfn "%i"

// part 2
let AdjacentGears (num:Num) = 
    let mutable ret = None
    for pos in num.Positions do
        let adjacentCells = adjacency_list |> Seq.filter (fun x -> x.Position = pos) |> Seq.map _.Adjacents |> Seq.exactlyOne
        let adjacentStars = adjacentCells |> Seq.filter (fun x -> lines.[(fst x)].[(snd x)] = '*') 
        let cellsAdjacentToStars = adjacentStars |> Seq.map (fun x -> adjacency_list |> Seq.filter (fun z -> z.Position = x) |> Seq.map _.Adjacents) |> Seq.collect id |> Seq.collect id // the adjacency list of those *s
        let num = cellsAdjacentToStars |> Seq.map (fun x -> 
            // for every number, get its positions. filter them to where the *s adjacency list contains the position. remove the original
            let matches = nums |> Seq.filter (fun t -> t.Positions |> Seq.exists (fun z -> z = x)) |> Seq.except [num] |> Seq.toList 
            match matches.Length with
                | 1 -> Some(matches |> Seq.exactlyOne)
                | _ -> None
        )
        let res = num |> Seq.choose id |> Seq.toList
        if res.Length > 0 then
            ret <- Some(res.[0])
    num,ret

nums 
    |> Seq.map AdjacentGears
    |> Seq.filter (fun x-> Option.isSome (snd x))
    |> Seq.map (fun x -> fst x, Option.get (snd x))
    |> Seq.distinctBy (fun x -> 
        let y = [fst x; snd x] |> List.sort
        y.[0],y.[1] 
    )
    |> Seq.map (fun x -> (fst x).Value * (snd x).Value) 
    |> Seq.sum
    |> printfn "%i"