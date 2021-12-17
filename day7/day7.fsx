#time

let input = "./day7/day7.txt" |> System.IO.File.ReadAllText |> fun s -> s.Split ',' |> Array.map int

// let a1 gt = [0 .. gt] |> Array.toArray
let a2 gt = [|0 .. gt|] |> Array.map (fun a -> Array.sum [|0 .. a|])

let range costs i = 
    [costs |> Array.take i |> Array.sortDescending ; costs |> Array.tail |> Array.take ((Array.max input) - i)] |> Array.concat

let getanswer a input =
    let gt = a (Array.max input)
    input |> Array.map (range gt) |> Array.transpose |> Array.minBy Array.sum |> Array.sum

// getanswer a1 input |> printfn "Answer: %A"
getanswer a2 input |> printfn "Answer: %A"
