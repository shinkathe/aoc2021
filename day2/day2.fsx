let toCommand (line: string) =
    line.Split ' '
    |> fun command -> command.[0], int command.[1]

let input =
    System.IO.File.ReadAllLines "day2.txt"
    |> Seq.map toCommand

let toInstruction1 instruction =
    match instruction with
    | ("up", up) -> fun (distance, depth) -> distance, depth - up
    | ("down", down) -> fun (distance, depth) -> distance, depth + down
    | (_, forward) -> fun (distance, depth) -> distance + forward, depth

let toInstruction2 instruction =
    match instruction with
    | ("up", up) -> fun (distance, depth, aim) -> distance, depth, aim - up
    | ("down", down) -> fun (distance, depth, aim) -> distance, depth, aim + down
    | (_, forward) -> fun (distance, depth, aim) -> distance + forward, depth + aim * forward, aim

let answer1 =
    input
    |> Seq.map toInstruction1
    |> Seq.fold ((fun acc instruction -> instruction acc)) (0, 0)

printfn
    "%A"
    (answer1
     |> fun (distance, depth) -> distance * depth)

let answer2 =
    input
    |> Seq.map toInstruction2
    |> Seq.fold (fun acc instruction -> instruction acc) (0, 0, 0)

printfn
    "%A"
    (answer2
     |> fun (distance, depth, _) -> distance * depth)
