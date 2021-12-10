let input =
    System.IO.File.ReadAllLines "./day6/day6.txt"
    |> Seq.head
    |> fun line -> line.Split(",")
    |> Seq.map int
    |> Seq.countBy (id)

let rec simFish day (fishByTimer: array<uint64>) =
    seq {
        let spawnDate = (day + 7) % 9
        fishByTimer.[spawnDate] <- fishByTimer.[spawnDate] + fishByTimer.[day % 9]

        yield fishByTimer
        yield! simFish (day + 1) fishByTimer
    }

Array.init 9 (fun f ->
    match Seq.tryFind (fst >> (=) f) input with
    | Some x -> x |> snd |> uint64
    | _ -> 0 |> uint64)
|> simFish 0
|> Seq.item 255 // insert 79 for answer to part 1
|> Seq.sum
|> printfn "fish %A"
