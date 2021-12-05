namespace AdventOfCode

open System
open System.IO
open Xunit
open Xunit.Abstractions

module Day5 =

    type Coordinates = { x: int; y: int }
    type Line = Coordinates * Coordinates

    let parseCoordinates (input: string) : Coordinates =
        let parts = input.Split ','

        { x = Convert.ToInt32 parts.[0]
          y = Convert.ToInt32 parts.[1] }

    let parseLine (input: string) : Line =
        let parts = input.Split ' '
        (parts |> Seq.head |> parseCoordinates, parts |> Seq.last |> parseCoordinates)

    let parseInput (input: seq<string>) : seq<Line> = input |> Seq.map parseLine

    let isHorizontalOrVertical ((a, b): Line) : bool = a.x = b.x || a.y = b.y

    type Tests(output: ITestOutputHelper) =

        let testDataInput =
            "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"

        let testData = testDataInput.Split '\n' |> parseInput

        [<Fact>]
        let ``Filters horizontal and vertical lines`` () =
            Assert.Equal(
                6,
                testData
                |> Seq.filter isHorizontalOrVertical
                |> Seq.length
            )

        [<Fact>]
        let ``Counts number of points with overlapping lines`` () =
            let result =
                File.ReadAllLines "data/day5input.txt"
                |> parseInput

            output.WriteLine(result.ToString())
