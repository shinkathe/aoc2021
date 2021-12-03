let input = System.IO.File.ReadAllLines "day1.txt" |> Seq.map int

let pairsWhereGt x = x |> Seq.pairwise |> Seq.map(fun (prev, curr) -> prev < curr) |> Seq.where ((=) true) |> Seq.length

let answer1 = input |> pairsWhereGt
printfn "%i" answer1 

let answer2 = input |> Seq.windowed 3 |> Seq.map Array.sum |> pairsWhereGt
printfn "%i" answer2 


