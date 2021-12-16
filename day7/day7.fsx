let fish 
    = "./day7/day7.txt" 
    |> System.IO.File.ReadAllText 
    |> fun s -> s.Split ',' 
    |> Array.map int

let gt = Seq.max fish

let a2 = [0 .. gt] |> Seq.map (fun a -> Seq.sum [0 .. a])
let a1 = seq [0 .. gt]

let range grt costs i = 
    [costs |> Seq.take i |> Seq.sortDescending ] @ [costs |> Seq.tail |> Seq.take (grt - i)] |> Seq.concat

let getanswer a =
    fish 
        |> Seq.map (range gt a) 
        |> Seq.transpose
        |> Seq.minBy Seq.sum
        |> Seq.sum
        |> printfn "Answer: %A"

getanswer a1
getanswer a2
