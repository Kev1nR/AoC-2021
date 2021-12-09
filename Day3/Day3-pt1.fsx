#load "../Read-data.fsx"

let filePath = @"Day3/input-data.txt"

//(ReadData.readLines filePath)
// [
//     "00100"
//     "11110"
//     "10110"
//     "10111"
//     "10101"
//     "01111"
//     "00111"
//     "11100"
//     "10000"
//     "11001"
//     "00010"
//     "01010"
// ]

let calcResult data =
    let dataWidth = (data |> Seq.head |> String.length) - 1
    let acc' = 0, [ for i in 0..dataWidth do yield 0 ]

    data
    |> Seq.map (fun s -> s.ToCharArray()) //Seq.map (accumulate')
    |> Seq.fold (fun acc el ->
                        (fst acc) + 1,
                        [
                            for i in 0..dataWidth do yield (snd acc)[i] + (if el[i] = '1' then 1 else 0)
                        ]
                    ) acc'
    |> fun (len, vals) -> [for i in 0..dataWidth do yield if vals[i] > (len/2) then 1 else 0]
    |> List.rev
    |> List.fold (fun (i,b,c) el ->
                        let el' = if el = 1 then 0 else 1

                        let summand = (float)el * (2.**(float i))
                        let summand' = (float)el' * (2.**(float i))

                        (i + 1, b + (int summand), c + (int summand'))
                 ) (0, 0, 0)

(ReadData.readLines filePath)
|> calcResult
|> fun (_, l, r) -> l * r
