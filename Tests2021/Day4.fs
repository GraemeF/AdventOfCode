namespace AdventOfCode

open System
open System.IO
open Xunit
open Xunit.Abstractions

module Day4 =

    type Board = Option<int> [] []

    type BingoState =
        { drawnNumbers: seq<int>
          boards: seq<Board> }

    let parseBoardRow (input: string) : seq<Option<int>> =
        input.Split ' '
        |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace s))
        |> Seq.map (fun n -> Some(Convert.ToInt32 n))

    let parseBoard (input: seq<string>) : Option<int> [] [] =
        input
        |> Seq.map (fun line -> parseBoardRow line |> Seq.toArray)
        |> Seq.toArray

    let allEqual =
        Array.forall2 (fun elem1 elem2 -> elem1 = elem2)

    let boardEqual (a: Board) (b: Board) =
        Seq.forall2 (fun a1 b1 -> allEqual a1 b1) a b

    let rec parseBoards (input: seq<string>) : List<Board> =
        if input |> Seq.isEmpty then
            []
        else
            let isBoardRow =
                fun (line: string) -> not (String.IsNullOrWhiteSpace line)

            let rows =
                input |> Seq.tail |> Seq.takeWhile isBoardRow

            let remainingRows =
                input |> Seq.tail |> Seq.skipWhile isBoardRow

            (parseBoard rows) :: (parseBoards remainingRows)

    let parseInput (input: seq<string>) : BingoState =
        { drawnNumbers =
              (input |> Seq.head).Split ','
              |> Seq.map Convert.ToInt32
          boards = input |> Seq.tail |> parseBoards }

    let markRow (number: int) (row: Option<int> []) : Option<int> [] =
        row
        |> Array.map
            (fun x ->
                if (x.IsSome && x.Value = number) then
                    None
                else
                    x)

    let markBoard (number: int) (board: Board) : Board = board |> Array.map (markRow number)

    let draw (state: BingoState) : BingoState =
        let drawnNumber = state.drawnNumbers |> Seq.head

        let boards: seq<Board> =
            state.boards |> Seq.map (markBoard drawnNumber)

        { drawnNumbers = state.drawnNumbers |> Seq.tail
          boards = boards }

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
        let ``Parses a board row`` () =
            Assert.Equal(([| 22; 13; 17; 11; 0 |] |> Array.map Some), "22 13 17 11  0" |> parseBoardRow)

        [<Fact>]
        let ``Parses a board`` () =
            let board =
                [ "22 13 17 11  0"
                  " 8  2 23  4 24"
                  "21  9 14 16  7"
                  " 6 10  3 18  5"
                  " 1 12 20 15 19" ]
                |> parseBoard

            let expected =
                [| [| 22; 13; 17; 11; 0 |]
                   [| 8; 2; 23; 4; 24 |]
                   [| 21; 9; 14; 16; 7 |]
                   [| 6; 10; 3; 18; 5 |]
                   [| 1; 12; 20; 15; 19 |] |]

            Assert.True(boardEqual (expected |> Array.map (Array.map Some)) board)

        [<Fact>]
        let ``Parses boards`` () =
            Assert.Equal(3, testData.boards |> Seq.length)

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
