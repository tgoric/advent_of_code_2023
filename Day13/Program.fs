open System
open System.IO

type Direction = 
    | Horizontal
    | Vertical

let DiffCount(x:char array)(y:char array):int = 
    let len = x.Length
    let mutable diffs = 0
    for i=0 to len-1 do
        if not(x[i] = y[i]) then diffs <- diffs + 1
    diffs

let FindPattern(smudge:bool) (s: string): int*Direction = 
    let rows = s.Split(Environment.NewLine) |> Seq.map (fun x -> x |> Seq.map id) |> array2D
    let permissibleDiffs = if smudge then 1 else 0
    let mutable i=0
    let mutable j=1
    let mutable ret=false
    while i < rows.GetLength(0)-1 && not(ret) do
        let mutable totalDiffs = DiffCount rows[i,*] rows[i+1,*]
        if totalDiffs <= permissibleDiffs then
            let mutable valid = true
            j <- 1
            while i-j >= 0 && i+j+1 <= rows.GetLength(0)-1 do
                totalDiffs <- totalDiffs + DiffCount rows[i-j,*] rows[i+j+1,*]
                valid <- valid && totalDiffs <= permissibleDiffs
                j <- j+1
            if valid && totalDiffs = permissibleDiffs then ret <- true
        i <- i+1

    if ret then
        i,Horizontal
    else
        i <- 0
        j <- 1
        ret <- false

        while i < rows.GetLength(1)-1 && not(ret) do
            let mutable totalDiffs = DiffCount rows[*,i] rows[*,i+1]
            if totalDiffs <= permissibleDiffs then
                let mutable valid = true
                j <- 1
                while i-j >= 0 && i+j+1 <= rows.GetLength(1)-1 do
                    totalDiffs <- totalDiffs + DiffCount rows[*,i-j] rows[*,i+j+1]
                    valid <- valid && totalDiffs <= permissibleDiffs
                    j <- j+1
                if valid && totalDiffs = permissibleDiffs then ret <- true
            i <- i+1
        i,Vertical

// part 1
File.ReadAllText("input.txt").Split(Environment.NewLine+Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun x -> FindPattern false x)
    |> Seq.map (fun x -> match snd x with | Vertical -> fst x | Horizontal -> 100*(fst x))
    |> Seq.sum
    |> printfn "%i"

// part 2
File.ReadAllText("input.txt").Split(Environment.NewLine+Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun x -> FindPattern true x)
    |> Seq.map (fun x -> match snd x with | Vertical -> fst x | Horizontal -> 100*(fst x))
    |> Seq.sum
    |> printfn "%i"