open System.IO

// shared
type Draw = {
    Reds: int
    Blues: int
    Greens: int
}

type Game = {
    Id: int
    Draws: Draw list
}

let (|Suffix|_|) (p:string) (s:string) =
    if s.EndsWith(p) then
        Some(s.Replace(p, "") |> int)
    else
        None

let ParseRow(row: string): Game = 
    let split = row.Split(':')
    let gameId = split.[0].Replace("Game ", "") |> int
    let draws = split.[1].Split(';') |> Seq.map (fun x -> 
            let mutable r = 0
            let mutable g = 0
            let mutable b = 0
            x.Split(',') 
                |> Seq.iter(fun y -> 
                        match y with
                            | Suffix " red" num -> r <- num
                            | Suffix " green" num -> g <- num
                            | Suffix " blue" num -> b <- num
                    )
            {Reds = r; Greens = g; Blues = b}
        )
    {Id = gameId; Draws = draws |> Seq.toList}

// part 1
let MaxRed = 12
let MaxGreen = 13
let MaxBlue = 14

let ValidDraw(d:Draw):bool = 
    d.Reds <= MaxRed &&
    d.Blues <= MaxBlue &&
    d.Greens <= MaxGreen

let ValidGame(g:Game):bool = 
    g.Draws 
        |> Seq.forall ValidDraw

File.ReadLines("input.txt")
    |> Seq.map ParseRow 
    |> Seq.filter ValidGame
    |> Seq.sumBy _.Id
    |> printfn "%i"


// part 2
let MinimalDraw(g:Game):Draw = 
    let r = g.Draws |> Seq.maxBy _.Reds
    let gr = g.Draws |> Seq.maxBy _.Greens
    let b = g.Draws |> Seq.maxBy _.Blues
    {Reds = r.Reds; Greens = gr.Greens; Blues = b.Blues}

File.ReadLines("input.txt")
    |> Seq.map ParseRow
    |> Seq.map MinimalDraw
    |> Seq.map (fun x -> x.Reds * x.Greens * x.Blues)
    |> Seq.sum
    |> printfn "%i"