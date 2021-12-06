#load "..\Read-data.fsx"

let filePath = @"source\AoC\Day1\input-data-full.txt"

(ReadData.readLines filePath) 
//|> Seq.take 8
|> Seq.map (int)
|> Seq.windowed 3
|> Seq.map (fun arr -> arr |> Array.sum)
|> Seq.pairwise
|> Seq.countBy (fun (l,r) -> r > l)
|> Seq.head
|> fun x -> printfn "Increase count is %d" (snd x)