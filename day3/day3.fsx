let log x =
    x |> Seq.iter (printfn "%A")
    x

let input =
    System.IO.File.ReadLines "day3.txt"
    |> Seq.map (fun (x) -> x.ToCharArray())

let inverse =
    fun bit ->
        match bit with
        | '1' -> '0'
        | _ -> '1'

let source = array2D input
let width = Array2D.length2 source - 1
let height = Array2D.length1 source - 1

let getColumn src col =
    [| 0 .. height |]
    |> Seq.map (fun row -> Array2D.get src row col)

let leastSig x =
    x
    |> Seq.groupBy (fun x -> x)
    |> Seq.minBy (fun (_, seq) -> seq |> Seq.length)
    |> fun (bit, _) -> bit

let gammaRate src =
    [| 0 .. width |]
    |> Seq.map (getColumn src)
    |> Seq.map leastSig

let gammaRate' = gammaRate source

let toDecimal (x: seq<char>) =
    x
    |> System.String.Concat
    |> fun s -> System.Convert.ToInt32(s, 2)

let gamma = gammaRate' |> toDecimal

let epsilonRate =
    gammaRate' |> Seq.map inverse |> toDecimal

printfn "Answer 1: %d" (epsilonRate * gamma)
