#load "..\Read-data.fsx"

let filePath = @"Day2\input-data.txt"

type Move =
| Forward of distance : int
| Up of distance      : int
| Down of distance    : int

let ParseMove (s : string) = 
    let dirDist = s.Split(' ')
    if dirDist[0] = "forward" then
        Forward (int dirDist[1])
    elif dirDist[0] = "up" then
        Up (int dirDist[1])
    elif dirDist[0] = "down" then
        Down (int dirDist[1])
    else
        failwith "Unknown direction"

(ReadData.readLines filePath) 
//|> Seq.take 8
|> Seq.map (ParseMove)// |> Seq.map (int)
|> Seq.fold (fun (h, v) move -> 
                match move with
                | Forward d -> (h + d, v)
                | Up d      -> (h, v - d)
                | Down d    -> (h, v + d)) (0, 0)
|> fun x -> printfn "Moved %d horizontally, %d vertically. Result: %d" (fst x) (snd x) ((fst x) * (snd x))