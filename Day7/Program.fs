open System.IO

type Card = { Name: string; Value: int }

let Cards: Card seq = [
    { Name="A"; Value=14 }
    { Name="K"; Value=13 }
    { Name="Q"; Value=12 }
    { Name="J"; Value=11 }
    { Name="T"; Value=10 }
    { Name="9"; Value=9 }
    { Name="8"; Value=8 }
    { Name="7"; Value=7 }
    { Name="6"; Value=6 }
    { Name="5"; Value=5 }
    { Name="4"; Value=4 }
    { Name="3"; Value=3 }
    { Name="2"; Value=2 }
]

let SortDesc sequence = sequence |> Seq.sortWith (fun x y -> compare y x)
let SortDescBy f sequence = sequence |> Seq.sortWith (fun x y -> compare (f y) (f x))

let (|IsGrouped|_|) (counts : int seq) cards  =
    let groups = cards |> Seq.countBy _.Value
    if (Seq.compareWith Operators.compare (groups |> Seq.map snd |> Seq.sortDescending) counts) = 0 then Some(counts) else None

type HandType = 
    | FiveOfAKind = 6
    | FourOfAKind = 5
    | FullHouse = 4
    | ThreeOfAKind = 3
    | TwoPair = 2
    | Pair = 1
    | HighCard = 0

type Hand = { Cards: Card seq; Type: HandType; Bid: int }

let SortHands(h1:Hand)(h2:Hand):int = 
    if h1.Type = h2.Type then
        let cmp = Seq.compareWith (fun x y -> if x.Value > y.Value then 1 elif x.Value < y.Value then -1 else 0) h1.Cards h2.Cards
        printfn "%A %A %i" (h1.Cards |> Seq.map _.Name |> Seq.fold (+) "") (h2.Cards |> Seq.map _.Name |> Seq.fold (+) "") cmp
        cmp
    elif (h1.Type |> int) > (h2.Type |> int) then 
        1
    else 
        -1

let DetermineType(c:Card seq): HandType = 
    match c with
        | IsGrouped [5] c -> HandType.FiveOfAKind
        | IsGrouped [4;1] c -> HandType.FourOfAKind
        | IsGrouped [3;2] c -> HandType.FullHouse
        | IsGrouped [3;1;1] c -> HandType.ThreeOfAKind
        | IsGrouped [2;2;1] c -> HandType.TwoPair
        | IsGrouped [2;1;1;1] c -> HandType.Pair
        | _ -> HandType.HighCard

let BuildHand(s: string):Hand = 
    let split = s.Split(" ")
    let c = split.[0] |> Seq.map (fun x -> Cards |> Seq.find (fun y -> y.Name = x.ToString()))
    let sorted = c |> Seq.sortByDescending _.Value
    {Cards = c; Type = DetermineType sorted; Bid = split.[1] |> int}

// part 1
File.ReadLines("input.txt")
    |> Seq.map BuildHand
    |> Seq.sortWith SortHands
    |> Seq.mapi (fun i x -> 
        printfn "%i %i" (x.Bid) (i+1)
        (i+1)*x.Bid
    )
    |> Seq.sum
    |> printfn "%i"