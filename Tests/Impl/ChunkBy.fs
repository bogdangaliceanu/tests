namespace ChunkBy

open System
open System.Collections.Generic

module MyList =
    let chunkBy (keySelector : ('T -> 'Key)) (source : 'T list) : ('Key * 'T list) list =
        let rec chunkByTR source totalAcc chunkAcc prevKey =
            match source with
            | [] -> ((prevKey, chunkAcc |> List.rev) :: totalAcc) |> List.rev
            | hd :: tl ->
                let currKey = keySelector hd
                if currKey = prevKey then
                    chunkByTR tl totalAcc (hd :: chunkAcc) prevKey
                else
                    chunkByTR tl ((prevKey, chunkAcc |> List.rev) :: totalAcc) [ hd ] currKey

        if source.IsEmpty then []
        else chunkByTR source [] [] (keySelector source.[0])
    
module MyArray =
    let chunkBy (keySelector : ('T -> 'Key)) (source : 'T[]) : ('Key * 'T[]) [] =
        if source.Length = 0 then [||]
        else
            let mutable prevKey = keySelector source.[0]
            let mutable currElements = new List<'T>()
            currElements.Add(source.[0])
            let result = new List<'Key * 'T[]>()

            for pos = 1 to source.Length - 1 do
                let currKey = keySelector source.[pos]
                if (currKey = prevKey) then currElements.Add(source.[pos])
                else
                    result.Add((prevKey, currElements.ToArray()))
                    prevKey <- currKey
                    currElements.Clear()
                    currElements.Add(source.[pos])

            result.Add((prevKey, currElements.ToArray()))
            result.ToArray()

module MySeq =
    let chunkBy (keySelector : ('T -> 'Key)) (source : seq<'T>) : seq<('Key * seq<'T>)> =
        seq {
            use e = source.GetEnumerator()
            if e.MoveNext() then
                let prevKey = ref (keySelector e.Current)
                let currElements = ref [ e.Current ]
                while e.MoveNext() do
                    let currKey = keySelector e.Current
                    if (currKey = !prevKey) then currElements := e.Current :: !currElements
                    else
                        yield (!prevKey, !currElements |> List.rev |> List.toSeq)
                        prevKey := currKey
                        currElements := [ e.Current ]

                yield (!prevKey, !currElements |> List.rev |> List.toSeq)
        }