open System
open System.IO
open System.Collections.Generic

// shared
let PerformCalc(words: IEnumerable<string>): int = 
    words
        |> Seq.map (fun x ->
            let nums = x |> Seq.filter Char.IsDigit
            let first = nums |> Seq.head
            let last = nums |> Seq.last
            ([first; last] |> Seq.map string |> String.concat "" |> int)
        ) 
        |> Seq.sum

// part 1
File.ReadLines("input.txt")
    |> PerformCalc
    |> printfn "%i"


// part2
let numbers = Map[
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9")
]

let entries = File.ReadLines("input.txt")

[
for entry in entries do
    let mutable str = entry
    for num in numbers do
        let mutable index = str.IndexOf(num.Key)
        // replace all occurrences of this number word in the string with the digit,
        // but preserve the first and last character of the word in place in case of
        // overlapping strings like threeight (this needs to count as 38, not 3ight)
        while index <> -1 do
            str <- str.Remove(index+1, num.Key.Length-2).Insert(index+1, num.Value)
            index <- str.IndexOf(num.Key)
    str
]
    |> PerformCalc
    |> printfn "%i"
