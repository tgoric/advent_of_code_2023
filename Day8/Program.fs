open System
open System.IO

let lines = File.ReadLines("input.txt")

type Direction = 
    | Left
    | Right

type Step = {
    Left: string
    Right: string
}

let Instructions = 
    lines
        |> Seq.head 
        |> Seq.map id 
        |> Seq.map (fun x -> match x with | 'L' -> Direction.Left |  _ -> Direction.Right)
        |> Seq.toList

let Directions = Map.ofSeq (
    lines
        |> Seq.skip 2 
        |> Seq.map (fun x ->
            let split = x.Split([|"="; ","|], StringSplitOptions.RemoveEmptyEntries)
            let start = split.[0].Trim()
            let left = split.[1].Replace("(","").Trim()
            let right = split.[2].Replace(")","").Trim()
            start, {Left = left; Right = right}
        )
)

let Travel(from: string) (direction: Direction) : string = 
    match direction with
        | Direction.Left -> Directions.[from].Left
        | Direction.Right -> Directions.[from].Right
        | _ -> failwith "bad input direction"

let instructionLength = Instructions |> Seq.length
let mutable steps = 0

// part 1
let mutable location = "AAA"
let dest = "ZZZ"
while not(location = dest) do
    let i = Instructions.[steps % instructionLength]
    steps <- steps + 1
    location <- Travel location i

printfn "%i" steps

// part 2
let gcf (a:int64) (b:int64):int64 =
    let mutable b2 = b
    let mutable a2 = a
    while not(b2 = 0L) do
        let temp = b2
        b2 <- a2 % b2
        a2 <- temp
    a2

let lcm (a:int64) (b:int64):int64 =
    (a / (gcf a b)) * b

let locations = Directions |> Seq.filter(fun x -> x.Key.EndsWith("A")) |> Seq.map(fun x -> x.Key)
let allsteps = [
    for location in locations do
        steps <- 0
        let mutable l = location
        while not(l.EndsWith("Z")) do
            let i = Instructions.[steps % instructionLength]
            steps <- steps + 1
            l <- Travel l i
        steps
]

let mutable total:int64 = 0
total <- lcm allsteps.[0] allsteps.[1]
for i in [2..5] do
    total <- lcm total allsteps.[i]

printfn "%i" total