namespace ChunkBy

open System

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
            let inline rev arr =
                let result = arr |> List.toArray
                Array.Reverse(result)
                result

            let mutable prevKey = keySelector source.[0]
            let mutable currElements = [ source.[0] ]
            let mutable result = []

            for pos = 1 to source.Length - 1 do
                let currKey = keySelector source.[pos]
                if (currKey = prevKey) then currElements <- source.[pos] :: currElements
                else
                    result <- (prevKey, rev currElements) :: result
                    prevKey <- currKey
                    currElements <- [ source.[pos] ]

            result <- (prevKey, rev currElements) :: result
            let resultArr = result |> List.toArray
            Array.Reverse(resultArr)
            resultArr

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