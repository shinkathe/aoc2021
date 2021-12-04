let toCommand (line: string) =
    line.Split ' '
    |> fun command -> command.[0], int command.[1]

let input =
    System.IO.File.ReadAllLines "day2.txt"
    |> Seq.map toCommand

let toInstruction1 instruction =
    fun (distance, depth) ->
        match instruction with
        | ("up", up) -> distance, depth - up
        | ("down", down) -> distance, depth + down
        | (_, forward) -> distance + forward, depth

let toInstruction2 instruction =
    fun (distance, depth, aim) ->
        match instruction with
        | ("up", up) -> distance, depth, aim - up
        | ("down", down) -> distance, depth, aim + down
        | (_, forward) -> distance + forward, depth + aim * forward, aim

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
