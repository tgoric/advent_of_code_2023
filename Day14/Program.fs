open System
open System.IO

let SlideN (g:char array2d) = 
    let mutable grid = g
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
    //printfn "%A \n" grid
    grid

let SlideS (g:char array2d) = 
    let mutable grid = g
    let rowlen = grid.GetLength(0)
    let collen = grid.GetLength(1)
    for i=rowlen-1 downto 0 do
        for j=collen-1 downto 0 do
            if grid[i,j] = 'O' then
                let mutable dest=i
                let mutable bail = false
                while dest<g.GetLength(0)-1 && not(bail) do
                    if grid[dest+1,j] = '.' then
                        dest <- dest + 1
                    else bail <- true
                grid[dest,j] <- 'O'
                if not(dest = i) then
                    grid[i,j] <- '.'
    //printfn "%A \n" grid
    grid

let SlideE (g:char array2d) = 
    let mutable grid = g
    let rowlen = grid.GetLength(0)
    let collen = grid.GetLength(1)
    for i=rowlen-1 downto 0 do
        for j=collen-1 downto 0 do
            if grid[i,j] = 'O' then
                let mutable dest=j
                let mutable bail = false
                while dest<g.GetLength(1)-1 && not(bail) do
                    if grid[i,dest+1] = '.' then
                        dest <- dest + 1
                    else bail <- true
                grid[i,dest] <- 'O'
                if not(dest = j) then
                    grid[i,j] <- '.'
    //printfn "%A \n" grid
    grid

let SlideW (g:char array2d) = 
    let mutable grid = g
    let rowlen = grid.GetLength(0)
    let collen = grid.GetLength(1)
    for i=0 to rowlen-1 do
        for j=0 to collen-1 do
            if grid[i,j] = 'O' then
                let mutable dest=j
                let mutable bail = false
                while dest>0 && not(bail) do
                    if grid[i,dest-1] = '.' then
                        dest <- dest - 1
                    else bail <- true
                grid[i,dest] <- 'O'
                if not(dest = j) then
                    grid[i,j] <- '.'
    //printfn "%A \n" grid
    grid

let Cycle g = 
    g
        |> SlideN
        |> SlideW
        |> SlideS
        |> SlideE

let Compute(x:char array2d): int = 
    let l = x.GetLength(0)
    let mutable total = 0
    for i=0 to l-1 do
        let count = x[i,*] |> Seq.sumBy (fun x -> if x='O' then 1 else 0)
        total <- total + ((l-i)*count)
    total
   
let GetString(x:char array2d):string = 
    let mutable s = ""
    for i=0 to x.GetLength(0)-1 do
        s <- s + new string(x[i,*])
    s
    //x |> Array.map(fun y -> y |> Seq.map id) |> Seq.map string |> String.concat " " //|> Seq.reduce (fun acc z -> acc + z.ToString()) //|> Seq.reduce (fun (acc:string) (z:char) -> acc + z.ToString())

let mutable g = File.ReadAllText("input.txt").Split(Environment.NewLine) |> Seq.map (fun x -> x |> Seq.map id) |> array2D
let mutable i = 0
let mutable size = 0
let mutable map:Map<string,int> = Map.empty
map <- map.Add(GetString(g), i)

let mutable bail = false
while i < 1000 && not(bail) do
    let x = Cycle g
    //printfn "%A \n" x
    let s = GetString(x)
    if map.ContainsKey(s) then
        //printfn "Loop detected at index %i with size %i" i (i-map[s])
        size <- i-map[s]
        bail <- true
    map <- map.Add(s, i)
    g <- x
    i <- i+1

g <- File.ReadAllText("input.txt").Split(Environment.NewLine) |> Seq.map (fun x -> x |> Seq.map id) |> array2D
let shortcut = ((1000000000 - i)%size)
//printfn "%i" shortcut
let mutable j=0
while j < shortcut+i do
    g <- Cycle g
    //Compute g |> printfn "%i"
    j<-j+1
Compute g |> printfn "%i"