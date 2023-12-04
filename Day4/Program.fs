open System
open System.IO

type Card = {
    Id: int
    WinningNums: int seq
    CardNums: int seq
}

type Winner = {
    CardId: int
    Winners: int seq
    Points: int
}

let ParseCard(input: string):Card = 
    let prefixSplit = input.Split(':')
    let id = prefixSplit.[0].Replace("Card ", "") |> int
    let nums = prefixSplit.[1].Split(" | ")
    let winners = nums.[0].Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun x -> x |> int)
    let cardNums = nums.[1].Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun x -> x |> int)
    { Id = id; WinningNums = winners; CardNums = cardNums }
    
let CheckWinners(c: Card): Winner = 
    let ws = c.CardNums |> Seq.filter (fun x -> c.WinningNums |> Seq.contains x)
    let points = pown 2 ((ws |> Seq.length)-1)
    {CardId = c.Id; Winners = ws; Points = points}

// part 1
File.ReadLines("input.txt")
    |> Seq.map ParseCard
    |> Seq.map CheckWinners
    |> Seq.sumBy _.Points
    |> printfn "%i"

// part 2
let allCards = File.ReadLines("input.txt") |> Seq.map ParseCard |> Seq.toList

let ExpandCards(cards: Card list): Card list = 
    let rec loop c remaining processed =
        let winners = (CheckWinners c).Winners |> Seq.length
        let maxCard = Math.Min(c.Id + winners, allCards.Length)
        let additionalCards = [|(c.Id+1) .. maxCard|] |> Seq.map (fun x -> allCards.[x-1]) |> Seq.toList
        match remaining, winners with
            | [], _ -> c :: processed
            | _, 0 -> loop (List.head remaining) (List.tail remaining) (c :: processed)
            | _, _ -> 
                let x = additionalCards @ remaining 
                loop (List.head x) (List.tail x) (c :: processed)
                
    loop (List.head cards) (List.tail cards) []

ExpandCards allCards |> Seq.length |> printfn "%i"