#load "..\Read-data.fsx"

let filePath = @"source\AoC\Day1\input-data.txt"

(ReadData.readLines filePath) 
|> Seq.map (int)
|> Seq.pairwise
|> Seq.countBy (fun (l,r) -> r > l)
|> Seq.head
|> fun x -> printfn "Increase count is %d" (snd x)