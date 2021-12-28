#load "../Read-data.fsx"
open System.Collections.Generic

let filePath = @"Day4/input-data-sample.txt"

let data = (ReadData.readLines filePath)

let getCallData data =
    let calls =
        data
        |> fun (s : string) -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun c -> c.ToString())
        |> Array.map (int)
    calls

type CardElement = {CardId: int; Row: int; Col: int; Value: int; Marked: bool; MarkedRowCount: int; MarkedColCount: int}

let rec buildCardStructures cardID rowID (cardIndexedDict : Dictionary<int, CardElement list>) (valIndexedDict : Dictionary<int, CardElement list>) data =
    let element =
        if data |> Seq.isEmpty then
            None
        else
            Some (data |> Seq.head)
    match element with
    | None -> cardIndexedDict
    | Some "" ->
        printfn "%d %A" (cardID + 1) (Seq.tail)
        buildCardStructures (cardID + 1) 0 cardIndexedDict valIndexedDict (data |> Seq.tail)
    | Some element' ->
        element' |> fun el -> el.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (int)
        |> Seq.iteri (fun i el ->
                        let ce = {CardId = cardID; Row = rowID; Col = i; Value = el; Marked = false; MarkedRowCount = 0; MarkedColCount = 0 }
                        printfn "CE %A" ce
                        if cardIndexedDict.ContainsKey(cardID) then
                            ce::cardIndexedDict[cardID] |> ignore
                        else
                            cardIndexedDict.Add(cardID, ce::[])

                        if valIndexedDict.ContainsKey(el) then
                            ce::valIndexedDict[el] |> ignore
                        else
                            valIndexedDict.Add(el, ce::[]))

        buildCardStructures cardID (rowID + 1) cardIndexedDict valIndexedDict (data |> Seq.tail)

let cd = new System.Collections.Generic.Dictionary<int, CardElement list>()
let vd = new System.Collections.Generic.Dictionary<int, CardElement list>()

data |> Seq.tail |> buildCardStructures 0 0 cd vd