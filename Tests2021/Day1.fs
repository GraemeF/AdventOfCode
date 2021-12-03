namespace AdventOfCode

open System
open System.IO
open Helpers
open Xunit
open Xunit.Abstractions

module Day1 =

    let IsDeeper a b = b > a

    let rec Sweep (last: Option<int>) (rest: seq<int>) : int =
        if rest |> Seq.exists (fun _ -> true) then
            0
        else
            let next = Some(rest |> Seq.head)

            (if last.IsSome && IsDeeper last next then
                 1
             else
                 0)
            + Sweep next (rest |> Seq.tail)

    let CountIncreases depths : int = Sweep None depths

    let rec SlidingWindow (depths: seq<int>) : list<int> =
        (depths |> Seq.take 3 |> Seq.sum)
        :: if depths |> Seq.length > 3 then
               (SlidingWindow depths) |> List.tail
           else
               []

    type Tests(output: ITestOutputHelper) =

        let testData =
            [ 199
              200
              208
              210
              200
              207
              240
              269
              260
              263 ]

        [<Fact>]
        let ``Increases are counted`` () =
            Assert.Equal(7, testData |> CountIncreases)

        [<Fact>]
        let ``Calculate sliding window increases`` () =
            Assert.Equal(5, testData |> SlidingWindow |> CountIncreases)

        [<Fact>]
        let ``Count sliding window totals`` () =
            (testData |> SlidingWindow)
            |> Helpers.shouldBeEquivalentsTo [ 607
                                               618
                                               618
                                               617
                                               647
                                               716
                                               769
                                               792 ]

        [<Fact>]
        let ``for reals`` () =
            let result =
                File.ReadAllLines "data/day1input.txt"
                |> Array.map Int32.Parse
                |> SlidingWindow
                |> CountIncreases

            output.WriteLine(result.ToString())
