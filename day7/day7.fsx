let fish 
    = "./day7/day7test.txt" 
    |> System.IO.File.ReadAllText 
    |> fun s -> s.Split ',' 
    |> Array.map int

let gt = Seq.max fish

let range grt i
    =  [i .. -1 .. 0] @ [1 .. grt - i]

//Answer 1
fish 
    |> Seq.map (range gt) 
    |> Seq.transpose
    |> Seq.minBy Seq.sum
    |> Seq.sum
    |> printfn "Answer 1: %A"
    