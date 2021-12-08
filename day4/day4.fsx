let log tx x =
    x |> Seq.iter (printfn "%s %A" tx)
    x

let input =
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
    |> Seq.map (fun board -> board, (board |> Seq.transpose))

let winningDraw (a, b) =
    Seq.concat [ a; b ]
    |> Seq.map (id >> snd |> Seq.maxBy)
    |> Seq.minBy (snd)
    |> snd

let boardsInOrderOfWinning =
    boardsWithSelectionData
    |> Seq.sortBy (winningDraw)
    |> log "test"

let unmarkedSum board =
    fst board
    |> Seq.concat
    |> Seq.where (snd >> (<) (winningDraw board))
    |> Seq.sumBy (fst)


let firstToWin = boardsInOrderOfWinning |> Seq.head
let lastToWin = boardsInOrderOfWinning |> Seq.last

let a1 =
    Seq.item (winningDraw firstToWin) selections
    * unmarkedSum firstToWin

let a2 =
    Seq.item (winningDraw lastToWin) selections
    * unmarkedSum lastToWin
