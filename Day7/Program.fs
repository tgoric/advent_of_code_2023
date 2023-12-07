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

let (|IsGrouped|_|) (counts : int seq) (jokers: bool) cards =
    let hasJoker = cards |> Seq.exists(fun x -> x.Value = 11)
    let groups =
        if jokers && hasJoker then
            let g = cards |> Seq.countBy _.Value
            let nonJokers = g |> Seq.filter(fun x -> not((fst x) = 11)) 
            match nonJokers with
                | x when Seq.isEmpty x -> Seq.ofList [(14,5)]
                | _ ->
                    let highest = nonJokers |> Seq.sortByDescending snd |> Seq.head |> fst
                    let jokers = g |> Seq.filter(fun x -> (fst x) = 11) |> Seq.exactlyOne |> snd
                    nonJokers |> Seq.map(fun x -> if (fst x) = highest then (fst x, snd x + jokers) else x)
        else
            cards |> Seq.countBy _.Value

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

let SortHands(jokers:bool)(h1:Hand)(h2:Hand):int = 
    if h1.Type = h2.Type then
        if jokers then
            Seq.compareWith (fun x y -> 
                let xv = if x.Value = 11 then 1 else x.Value
                let yv = if y.Value = 11 then 1 else y.Value
                if xv > yv then 1 elif xv < yv then -1 else 0
            ) h1.Cards h2.Cards
        else
            Seq.compareWith (fun x y -> if x.Value > y.Value then 1 elif x.Value < y.Value then -1 else 0) h1.Cards h2.Cards
    elif (h1.Type |> int) > (h2.Type |> int) then 
        1
    else 
        -1

let DetermineType(c:Card seq)(j:bool): HandType = 
    match c with
        | IsGrouped [5] j c -> HandType.FiveOfAKind
        | IsGrouped [4;1] j c -> HandType.FourOfAKind
        | IsGrouped [3;2] j c  -> HandType.FullHouse
        | IsGrouped [3;1;1] j c -> HandType.ThreeOfAKind
        | IsGrouped [2;2;1] j c -> HandType.TwoPair
        | IsGrouped [2;1;1;1] j c -> HandType.Pair
        | _ -> HandType.HighCard

let BuildHand(jokers: bool)(s: string):Hand = 
    let split = s.Split(" ")
    let c = split.[0] |> Seq.map (fun x -> Cards |> Seq.find (fun y -> y.Name = x.ToString()))
    let sorted = c |> Seq.sortByDescending _.Value
    {Cards = c; Type = DetermineType sorted jokers; Bid = split.[1] |> int}

let Compute(withJokers: bool) = 
   File.ReadLines("input.txt")
    |> Seq.map (BuildHand withJokers)
    |> Seq.sortWith (SortHands withJokers)
    |> Seq.mapi (fun i x -> (i+1)*x.Bid)
    |> Seq.sum
    |> printfn "%i" 

// part 1
Compute false

// part 2
Compute true