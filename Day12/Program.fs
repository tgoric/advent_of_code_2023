open System
open System.IO

let mutable cache:Map<string, int64> = Map.empty

let skipChars (n:int) (s:string) : string = new string(Seq.skip n s |> Seq.toArray)
let takeChars (n:int) (s:string) : string = new string(Seq.take n s |> Seq.toArray)

let rec Calculate(s: string) (g: int list): int64 = 
    let mutable springs = s
    let mutable groups = g
    let mutable ret = -1L
    while ret = -1L do
        if groups.Length = 0 then
            if springs.Contains("#") then ret <- 0 else ret <- 1
        elif String.IsNullOrWhiteSpace(s) then ret <- 0
        else
            if springs.StartsWith(".") then 
                springs <- springs.TrimStart('.')
            elif springs.StartsWith("?") then
                let skipped = skipChars 1 springs
                ret <- GetValue ("." + skipped) groups + GetValue ("#" + skipped) groups
            else
                if groups.Length = 0 then ret <- 0
                elif springs.Length < groups[0] then ret <- 0
                elif (takeChars groups[0] springs).Contains('.') then ret <- 0
                elif groups.Length > 1 then
                    if springs.Length < (groups[0] + 1) || springs[groups[0]] = '#' then ret <- 0
                    else
                        springs <- skipChars (groups[0]+1) springs
                        groups <- groups |> List.skip 1
                else
                    springs <- skipChars groups[0] springs
                    groups <- groups |> List.skip 1
    ret

and GetValue(s: string) (g: int list): int64 = 
    let key = s+","+String.Join(",", g)
    if cache.ContainsKey(key) then
        cache[key]
    else
        let res = Calculate s g
        cache <- cache.Add(key,res)
        res

let input = File.ReadAllLines("input.txt") |> Seq.map(fun x -> x.Split(' '))

// part 1
let mutable res = 0L
for i in input do
    let springs = i[0]
    let groups = i[1].Split(',') |> Seq.map int |> Seq.toList
    res <- res + Calculate springs groups
printfn "%i" res

// part 2
res <- 0L
for i in input do
    let springs = String.Join("?", Seq.replicate 5 i[0])
    let g = i[1].Split(',') |> Seq.map int |> Seq.toList
    let groups = List.replicate 5 g |> List.collect id
    res <- res + Calculate springs groups
printfn "%i" res