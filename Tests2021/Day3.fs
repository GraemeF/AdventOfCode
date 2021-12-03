namespace AdventOfCode

open System
open System.IO
open Helpers
open Xunit
open Xunit.Abstractions

module Day3 =

    let findGammaRate (diagnosticReport: seq<string>) =
        let length = (diagnosticReport |> Seq.head).Length
        length

    type Tests(output: ITestOutputHelper) =

        let testData =
            "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

        [<Fact>]
        let ``Finds gamma rate`` () =
            Assert.Equal(22, testData.Split '\n' |> findGammaRate)
