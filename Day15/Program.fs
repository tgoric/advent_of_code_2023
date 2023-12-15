open System.IO

File.ReadAllText("input.txt").Split(",") 
    |> Seq.map (fun x -> 
            let mutable curr = 0
            for i in x do
                curr <- ((curr + (i |> int)) * 17) % 256
            curr
        )
    |> Seq.sum
    |> printfn "%i"