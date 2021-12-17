let input = "./day7/day7.txt" |> System.IO.File.ReadAllText |> fun s -> s.Split ',' |> Array.map int

let a1 gt = seq [0 .. gt]
let a2 gt = [0 .. gt] |> Seq.map (fun a -> Seq.sum [0 .. a])

let range costs i = 
    [costs |> Seq.take i |> Seq.sortDescending ; costs |> Seq.tail |> Seq.take ((Seq.max input) - i)] |> Seq.concat

let getanswer a input =
    let gt = a (Seq.max input)
    input |> Seq.map (range gt) |> Seq.transpose |> Seq.minBy Seq.sum |> Seq.sum

getanswer a1 input |> printfn "Answer: %A"
getanswer a2 input |> printfn "Answer: %A"
