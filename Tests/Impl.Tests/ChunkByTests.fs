namespace Impl.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ChunkBy

[<TestClass>]
type List_chunkBy_Tests() = 
    [<TestMethod>]
    member this.``given empty list returns empty list``() =
        let result = MyList.chunkBy (fun x -> x) []
        Assert.IsTrue(result.IsEmpty)

    [<TestMethod>]
    member this.``given list with 1 element returns list with 1 tuple and 1 element in list``() =
        let result = MyList.chunkBy (fun x -> x) [ 0 ]
        Assert.AreEqual([ (0, [ 0 ]) ], result)

    [<TestMethod>]
    member this.``given list with several elements yielding same key returns list with 1 tuple and all elements in list``() =
        let result = MyList.chunkBy (fun x -> x % 2) [ 3; 5; 7 ]
        Assert.AreEqual([ (1, [ 3; 5; 7 ]) ], result)

    [<TestMethod>]
    member this.``given list with several elements each yielding different key returns list with several tuples and 1 element in each list``() =
        let result = MyList.chunkBy (fun x -> 8 - x) [ 3; 5; 7 ]
        Assert.AreEqual([ (5, [ 3 ]); (3, [ 5 ]); (1, [ 7 ]) ], result)

    [<TestMethod>]
    member this.``given list with several elements several consecutive yielding same key returns list with several tuples and several elements in at least 1 list``() =
        let result = MyList.chunkBy (fun x -> 8 - abs(x)) [ 3; -3; 5; 7 ]
        Assert.AreEqual([ (5, [ 3; -3 ]); (3, [ 5 ]); (1, [ 7 ]) ], result)

    [<TestMethod>]
    member this.``given list with several elements several nonconsecutive yielding same key returns list with several tuples and 1 element in each list``() =
        let result = MyList.chunkBy (fun x -> 8 - abs(x)) [ 3; 5; -3; 7 ]
        Assert.AreEqual([ (5, [ 3 ]); (3, [ 5 ]); (5, [ -3 ]); (1, [ 7 ]) ], result)

[<TestClass>]
type Array_chunkBy_Tests() = 
    [<TestMethod>]
    member this.``given empty array returns empty array``() =
        let result = MyArray.chunkBy (fun x -> x) [||]
        Assert.IsTrue(result.Length = 0)

    [<TestMethod>]
    member this.``given array with 1 element returns array with 1 tuple and 1 element in array``() =
        let result = MyArray.chunkBy (fun x -> x) [| 0 |]
        Assert.IsTrue((result = [| (0, [| 0 |]) |]))

    [<TestMethod>]
    member this.``given array with several elements yielding same key returns array with 1 tuple and all elements in array``() =
        let result = MyArray.chunkBy (fun x -> x % 2) [| 3; 5; 7 |]
        Assert.IsTrue((result = [| (1, [| 3; 5; 7 |]) |]))

    [<TestMethod>]
    member this.``given array with several elements each yielding different key returns array with several tuples and 1 element in each array``() =
        let result = MyArray.chunkBy (fun x -> 8 - x) [| 3; 5; 7 |]
        Assert.IsTrue((result = [| (5, [| 3 |]); (3, [| 5 |]); (1, [| 7 |]) |]))

    [<TestMethod>]
    member this.``given array with several elements several consecutive yielding same key returns array with several tuples and several elements in at least 1 array``() =
        let result = MyArray.chunkBy (fun x -> 8 - abs(x)) [| 3; -3; 5; 7 |]
        Assert.IsTrue((result = [| (5, [| 3; -3 |]); (3, [| 5 |]); (1, [| 7 |]) |]))

    [<TestMethod>]
    member this.``given array with several elements several nonconsecutive yielding same key returns array with several tuples and 1 element in each array``() =
        let result = MyArray.chunkBy (fun x -> 8 - abs(x)) [| 3; 5; -3; 7 |]
        Assert.IsTrue((result = [| (5, [| 3 |]); (3, [| 5 |]); (5, [| -3 |]); (1, [| 7 |]) |]))

[<TestClass>]
type Seq_chunkBy_Tests() = 
    let deepEnumerate s =
        s
        |> Seq.map (fun (a, b) -> (a, b |> Seq.toArray))
        |> Seq.toArray

    [<TestMethod>]
    member this.``given empty seq returns empty seq``() =
        let result = MySeq.chunkBy (fun x -> x) Seq.empty
        Assert.IsTrue(Seq.isEmpty result)

    [<TestMethod>]
    member this.``given seq with 1 element returns seq with 1 tuple and 1 element in seq``() =
        let result = MySeq.chunkBy (fun x -> x) (seq { yield 0 }) |> deepEnumerate
        let expected = seq { yield (0, seq { yield 0 }) } |> deepEnumerate
        Assert.IsTrue((result = expected))

    [<TestMethod>]
    member this.``given seq with several elements yielding same key returns seq with 1 tuple and all elements in seq``() =
        let result = MySeq.chunkBy (fun x -> x % 2) (seq { yield 3; yield 5; yield 7 }) |> deepEnumerate
        let expected = seq { yield (1, seq { yield 3; yield 5; yield 7 }) } |> deepEnumerate
        Assert.IsTrue((result = expected))

    [<TestMethod>]
    member this.``given seq with several elements each yielding different key returns seq with several tuples and 1 element in each seq``() =
        let result = MySeq.chunkBy (fun x -> 8 - x) (seq { yield 3; yield 5; yield 7 }) |> deepEnumerate
        let expected = seq { yield (5, seq { yield 3 }); yield (3, seq { yield 5 }); yield (1, seq { yield 7 }) } |> deepEnumerate
        Assert.IsTrue((result = expected))

    [<TestMethod>]
    member this.``given seq with several elements several consecutive yielding same key returns seq with several tuples and several elements in at least 1 seq``() =
        let result = MySeq.chunkBy (fun x -> 8 - abs(x)) (seq { yield 3; yield -3; yield 5; yield 7 }) |> deepEnumerate
        let expected = seq { yield (5, seq { yield 3;  yield -3 });
                             yield (3, seq { yield 5 });
                             yield (1, seq { yield 7 })
                       } |> deepEnumerate
        Assert.IsTrue((result = expected))

    [<TestMethod>]
    member this.``given seq with several elements several nonconsecutive yielding same key returns seq with several tuples and 1 element in each seq``() =
        let result = MySeq.chunkBy (fun x -> 8 - abs(x)) (seq { yield 3; yield 5; yield -3; yield 7 }) |> deepEnumerate
        let expected = seq { yield (5, seq { yield 3 });
                             yield (3, seq { yield 5 });
                             yield (5, seq { yield -3 });
                             yield (1, seq { yield 7 })
                       } |> deepEnumerate
        Assert.IsTrue((result = expected))