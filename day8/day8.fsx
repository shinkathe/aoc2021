// Answer 1
let input = 
    System.IO.File.ReadAllLines "./day8/day8.txt" |> Seq.map ((fun (s:string) -> s.Split " | ") >> Seq.map (fun (x:string) -> x.Split " "));

// Answer 1
input 
|> Seq.map (Seq.last)  
|> Seq.concat
|> Seq.where (fun x -> x.Length < 5 || x.Length = 7)
|> Seq.length
|> printfn "%A"

