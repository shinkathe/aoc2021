
let toTuple a = ( a |> Seq.item 0, a |> Seq.item 1 |> int )
let input = System.IO.File.ReadAllLines "day2.txt" |> Seq.map (fun x -> x.Split " ") |> Seq.map toTuple
let convertToInstructions x =
    match x with
    | ("up", up) -> fun (x, y) -> (x, y - up)
    | ("down", down) -> fun (x, y) -> (x, y + down)
    | ("forward", forward) -> fun (x, y) -> (x + forward, y)
    | (_, _) -> fun (x, y) -> (x, y)

let convertToInstructionsAnswer2 x =
    match x with
    | ("up", up) -> fun (x,y,aim) -> (x, y, aim - up)
    | ("down", down) -> fun (x,y,aim) -> (x, y, aim + down)
    | ("forward", forward) -> fun (x,y,aim) -> (x + forward, y + aim * forward, aim)
    | (_, _) -> fun (x,y,aim) -> (x,y,aim)

let answer1 = input |> Seq.map convertToInstructions |> Seq.fold (fun acc instruction -> instruction acc) (0,0) 
printfn "%A" (answer1 |> fun (x, y) -> x * y)

let answer2 = input |> Seq.map convertToInstructionsAnswer2 |> Seq.fold (fun acc instruction -> instruction acc) (0, 0, 0)
printfn "%A" (answer2 |> fun (x, y, _) -> x * y)