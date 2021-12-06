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
|> Seq.map (ParseMove)// |> Seq.map (int)
|> Seq.fold (fun (h, v, aim) move -> 
                match move with
                | Forward d -> (h + d, v + (aim * d), aim)
                | Up d      -> (h, v, aim - d)
                | Down d    -> (h, v, aim + d)) (0, 0, 0)
|> fun (h,v, a) -> printfn "Moved %d horizontally, %d vertically, aim is %d. Result: %d" h v a (h * v)