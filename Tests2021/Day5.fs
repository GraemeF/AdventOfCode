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

    let getIncrement a b : int =
        if a = b then 0
        else if a < b then 1
        else -1

    let rec getPoints ((a, b): Line) : seq<Coordinates> =
        if a = b then
            [ a ]
        else
            a
            :: (getPoints (
                    { x = a.x + getIncrement a.x b.x
                      y = a.y + getIncrement a.y b.y },
                    b
                )
                |> Seq.toList)

    let countCoveringPoints (points: seq<Coordinates>) : Map<Coordinates, int> =
        let increment =
            fun (count: Option<int>) -> Some((if count.IsSome then count.Value else 0) + 1)

        points
        |> Seq.fold (fun (map: Map<Coordinates, int>) point -> map.Change(point, increment)) Map.empty

    let countLinesCoveringPoints (lines: seq<Line>) : Map<Coordinates, int> =
        lines
        |> Seq.map getPoints
        |> Seq.concat
        |> countCoveringPoints

    let countPointsCoveredBy2OrMoreLines (lines: seq<Line>) =
        lines
        |> countLinesCoveringPoints
        |> Map.values
        |> Seq.filter (fun n -> n > 1)
        |> Seq.length

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
        let ``Gets points covered by a diagonal downward line`` () =
            Assert.Equal(
                [ { x = 1; y = 1 }
                  { x = 2; y = 2 }
                  { x = 3; y = 3 } ],
                ({ x = 1; y = 1 }, { x = 3; y = 3 }) |> getPoints
            )

        [<Fact>]
        let ``Gets points covered by a diagonal upward line`` () =
            Assert.Equal(
                [ { x = 9; y = 7 }
                  { x = 8; y = 8 }
                  { x = 7; y = 9 } ],
                ({ x = 9; y = 7 }, { x = 7; y = 9 }) |> getPoints
            )

        [<Fact>]
        let ``Gets points covered by a horizontal line`` () =
            Assert.Equal(
                [ { x = 1; y = 1 }
                  { x = 1; y = 2 }
                  { x = 1; y = 3 } ],
                ({ x = 1; y = 1 }, { x = 1; y = 3 }) |> getPoints
            )

        [<Fact>]
        let ``Gets points covered by a vertical line`` () =
            Assert.Equal(
                [ { x = 9; y = 7 }
                  { x = 8; y = 7 }
                  { x = 7; y = 7 } ],
                ({ x = 9; y = 7 }, { x = 7; y = 7 }) |> getPoints
            )

        [<Fact>]
        let ``Counts number of times points are hit`` () =
            let result =
                [ { x = 2; y = 2 }
                  { x = 2; y = 2 }
                  { x = 2; y = 2 }
                  { x = 1; y = 2 } ]
                |> countCoveringPoints

            Assert.Equal(2, result.Count)
            Assert.Equal(3, result.Item({ x = 2; y = 2 }))
            Assert.Equal(1, result.Item({ x = 1; y = 2 }))

        [<Fact>]
        let ``Counts number of points hit by 2 or more horizontal or vertical lines`` () =
            Assert.Equal(
                5,
                testData
                |> Seq.filter isHorizontalOrVertical
                |> countPointsCoveredBy2OrMoreLines
            )

        [<Fact>]
        let ``Counts number of points hit by 2 or more lines`` () =
            Assert.Equal(12, testData |> countPointsCoveredBy2OrMoreLines)

        [<Fact>]
        let ``Counts number of points with overlapping horizontal or vertical lines with real data`` () =
            let result =
                File.ReadAllLines "data/day5input.txt"
                |> parseInput
                |> Seq.filter isHorizontalOrVertical
                |> countPointsCoveredBy2OrMoreLines

            output.WriteLine(result.ToString())

        [<Fact>]
        let ``Counts number of points with overlapping lines with real data`` () =
            let result =
                File.ReadAllLines "data/day5input.txt"
                |> parseInput
                |> countPointsCoveredBy2OrMoreLines

            output.WriteLine(result.ToString())
