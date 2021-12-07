let log tx x =
    x |> Seq.iter (printfn "%s %A" tx)
    x

let input =
    System.IO.File.ReadAllLines "./day3/day3.txt"
    |> Seq.map id

type Bit =
    | Oxygen
    | CO2

let rec getByBit pos bit input =
    let sortByRatingType =
        match bit with
        | Oxygen ->
            Seq.sortByDescending (fst)
            >> Seq.maxBy (snd >> Seq.length)
        | CO2 -> Seq.sortBy (fst) >> Seq.minBy (snd >> Seq.length)

    match Seq.length input with // I'm pretty sure there is a better way than this
    | 1 -> input
    | _ ->
        input
        |> Seq.groupBy (id >> Seq.item pos)
        |> sortByRatingType
        |> snd
        |> getByBit (pos + 1) bit // We could also just calculate both branches at the same pass

let result =
    [ Oxygen; CO2 ]
    |> Seq.map (fun x -> getByBit 0 x input)
    |> Seq.map (
        System.String.Concat
        >> fun x -> System.Convert.ToInt32(x, 2)
    )
    |> Seq.reduce (*)

printfn "%A" result
