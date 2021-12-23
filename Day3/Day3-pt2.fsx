#load "../Read-data.fsx"

let filePath = @"Day3/input-data.txt"

let splitString data =
    let dataWidth = (data |> Seq.head |> String.length) - 1
    let acc' = 0, [ for i in 0..dataWidth do yield 0 ]

    data
    |> Seq.map (fun s -> s.ToCharArray())

let getFlagCounts level readings =
    let binCounts =
        readings
        |> Seq.countBy (fun el -> el |> Seq.skip level |> Seq.head )
    printfn "bincounts %A" binCounts
    binCounts

let getCount precedenceValue op counts =
    counts
    |> Seq.toList
    |> fun cs ->
        match cs with
        | a::b::[] ->
            if (snd a) = (snd b)
            then
                precedenceValue
            elif op (snd a) (snd b)
            then
                fst a
            else
                fst b
        | _ -> failwith "Unexpected char count"


let rec getOxyCOGen searchChar op level (readings : char[] seq) =
    let filterKey =
        readings |> getFlagCounts level |> getCount searchChar op

    let levelReadings =
        readings
        |> Seq.filter (fun reading -> reading[level] = filterKey)

    printfn "%d: %d %A" level (levelReadings |> Seq.length) levelReadings
    match levelReadings |> Seq.length with
    | 0 | 1  -> levelReadings |> Seq.head
    | _ -> getOxyCOGen searchChar op (level + 1) levelReadings

let getOxyGen level readings = getOxyCOGen '1' (>) level readings
let getCOGen level readings = getOxyCOGen '0' (<) level readings

let calcDecVal data =
    data
    |> Array.rev
    |> Array.fold (fun (i,b) el ->
                        let el' = if el = '0' then 0 else 1
                        let summand' = (float)el' * (2.**(float i))

                        let acc = (i + 1, b + (int summand'))
                        printfn "acc %A" acc
                        acc
                 ) (0, 0)

//(ReadData.readLines filePath)
let oxy () =
    [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ]
    |> splitString
    |> getOxyGen 0
    |> calcDecVal

let CO () =
    [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ]
    |> splitString
    |> getCOGen 0
    |> calcDecVal

// |> getFlagCounts
// |> getMaxCount



// |> calcResult
// |> fun (_, l, r) -> l * r
