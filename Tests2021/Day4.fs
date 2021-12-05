namespace AdventOfCode

open System
open System.IO
open Xunit
open Xunit.Abstractions

module Day4 =

    type Input = { drawnNumbers: seq<int> }

    let parseInput (input: seq<string>) : Input =
        { drawnNumbers =
              (input |> Seq.head).Split ','
              |> Seq.map Convert.ToInt32 }

    let getFinalScore input = 0

    type Tests(output: ITestOutputHelper) =

        let testDataInput =
            "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"

        let testData = testDataInput.Split '\n' |> parseInput

        [<Fact>]
        let ``Parses drawn numbers`` () =
            Assert.Equal(
                [ 7
                  4
                  9
                  5
                  11
                  17
                  23
                  2
                  0
                  14
                  21
                  24
                  10
                  16
                  13
                  6
                  15
                  25
                  12
                  22
                  18
                  20
                  8
                  19
                  3
                  26
                  1 ],
                testData.drawnNumbers
            )

        [<Fact>]
        let ``Calculates final score`` () =
            Assert.Equal(4512, testData |> getFinalScore)

        [<Fact>]
        let ``Calculates final score with real data`` () =
            let result =
                File.ReadAllLines "data/day4input.txt"
                |> parseInput
                |> getFinalScore

            output.WriteLine(result.ToString())
