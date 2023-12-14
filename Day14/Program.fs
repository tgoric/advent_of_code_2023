open System
open System.IO

let SlideNorth(s: string) = 
    let mutable grid = s.Split(Environment.NewLine) |> Seq.map (fun x -> x |> Seq.map id) |> array2D
    let rowlen = grid.GetLength(0)
    let collen = grid.GetLength(1)
    for i=0 to rowlen-1 do
        for j=0 to collen-1 do
            if grid[i,j] = 'O' then
                let mutable dest=i
                let mutable bail = false
                while dest>0 && not(bail) do
                    if grid[dest-1,j] = '.' then
                        dest <- dest - 1
                    else bail <- true
                grid[dest,j] <- 'O'
                if not(dest = i) then
                    grid[i,j] <- '.'

    grid

let Compute(x:char array2d): int = 
    let l = x.GetLength(0)
    let mutable total = 0
    for i=0 to l-1 do
        let count = x[i,*] |> Seq.sumBy (fun x -> if x='O' then 1 else 0)
        total <- total + ((l-i)*count)
    total
        
File.ReadAllText("input.txt")
    |> SlideNorth 
    |> Compute
    |> printfn "%i"