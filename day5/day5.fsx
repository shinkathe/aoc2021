let isHV x1 x2 y1 y2 = x1 = x2 || y1 = y2

let isInHVLine (x1, y1, x2, y2) (x, y) =
    x >= min x1 x2
    && x <= max x1 x2
    && y >= min y1 y2
    && y <= max y1 y2

let isWithin (x1, y1, x2, y2) (x, y) =
    match (isHV x1 x2 y1 y2) with
    | true when isInHVLine (x1, y1, x2, y2) (x, y) -> 1
    | false ->
        let dx = ((x - x1) |> double) / ((x2 - x1) |> double)
        let dy = ((y - y1) |> double) / ((y2 - y1) |> double)
        if dx = dy && dx >= 0. && dx <= 1. then 1 else 0
    | _ -> 0

let input =
    System.IO.File.ReadAllLines "./day5/day5.txt"
    |> Array.map (fun line ->
        line.Split([| " -> "; "," |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> fun cd -> isWithin (cd.[0], cd.[1], cd.[2], cd.[3]))

// Just to be clear, this is a terrible way to solve this - I'm doing it for fun :)
let coords =
    ([| 0 .. 1000 |], [| 0 .. 1000 |])
    ||> Array.allPairs
    |> Array.map (fun coord -> Seq.sumBy (fun fn -> fn coord) input)
    |> Array.countBy (fun f -> f > 1)

coords |> printfn "%A"
