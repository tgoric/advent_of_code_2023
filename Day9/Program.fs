open System.IO

let data = 
    File.ReadLines("input.txt")
        |> Seq.map (fun x -> x.Split(" ") |> Seq.map int |> Seq.toList)

let BuildPyramid(line: int list) = 
    let rec loop (acc: int list list) (xs:int list) =
        match xs |> Seq.forall (fun x -> x = 0) with
            | true -> 
                let x = (List.init (xs |> Seq.length) (fun x -> 0))
                acc @ [x]
            | false -> 
                let next = [0..((xs |> Seq.length) - 2)] |> Seq.map(fun x -> (xs.[x+1]) - (xs.[x])) |> Seq.toList
                loop (acc @ [xs]) next

    loop [] line |> List.rev

let SolveEach(prepend: bool) = 
    [for line in data do
        let pyramid = BuildPyramid line

        let mutable changed: int list list = []
        let mutable idx = 0
        for p in pyramid do
            if idx = 0 then 
                if prepend then
                    changed <- changed @ [0 :: p]
                else
                    changed <- changed @ [p @ [0]]
            else
                if prepend then
                    let prior = Seq.head changed.[idx-1]
                    let x = (p.[0]) - prior
                    changed <- changed @ [x :: p]
                else
                    let prior = Seq.head (Seq.rev changed.[idx-1])
                    let p' = List.rev p
                    let x = (p'.[0]) + prior
                    changed <- changed @ [p @ [x]]
            idx <- idx + 1
        
        if prepend then
            changed |> Seq.map (fun x -> Seq.head x) |> Seq.last
        else
            changed |> Seq.map (fun x -> Seq.last x) |> Seq.last
    ]

// part 1
SolveEach false |> Seq.sum |> printfn "%i"

// part 2
SolveEach true |> Seq.sum |> printfn "%i"