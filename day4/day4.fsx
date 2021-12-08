let log tx x =
    x |> Seq.iter (printfn "%s %A" tx)
    x

let input =
    // System.IO.File.ReadAllLines "./day4/day4.txt"
    System.IO.File.ReadAllLines "./day4/day4.txt"
    |> Seq.map (fun line -> line.Split ' ')
    |> Seq.map (id >> Seq.map int)

let selections = Seq.head input

let boards = input |> Seq.tail

let boardWidth = 5

let boardsWithSelectionData =
    boards
    |> Seq.map (
        id
        >> Seq.map (fun bval -> bval, (Seq.findIndex ((=) bval) selections))
        >> Seq.splitInto boardWidth
        >> Seq.map seq
    )

let lowestAnswers =
    id
    >> Seq.minBy (id >> Seq.sumBy (snd))
    >> Seq.maxBy snd
    >> snd

let winningBoard =
    Seq.concat [ boardsWithSelectionData
                 boardsWithSelectionData
                 |> Seq.map (id >> Seq.transpose >> Seq.map seq) ]
    |> Seq.sortBy lowestAnswers
    |> Seq.head
    |> log "test"

let lowestDraw = winningBoard |> lowestAnswers

let unmarked =
    winningBoard
    |> Seq.concat
    |> Seq.where (snd >> (<) lowestDraw)
    |> Seq.sumBy fst

let lastCall =
    winningBoard
    |> Seq.concat
    |> Seq.find (snd >> (=) lowestDraw)
    |> fst

printfn "%A" (unmarked * lastCall)
