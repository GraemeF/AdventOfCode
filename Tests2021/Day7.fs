namespace AdventOfCode

open System
open System.IO
open Xunit
open Xunit.Abstractions

module Day7 =

    type HorizontalPosition = int

    let parseInput (input: string) =
        input.Split ',' |> Seq.map Convert.ToInt32

    let countCrabsAtEachPosition (positions: seq<HorizontalPosition>) : Map<HorizontalPosition, int> =
        let increment =
            fun (count: Option<int>) -> Some((if count.IsSome then count.Value else 0) + 1)

        positions
        |> Seq.fold (fun (map: Map<HorizontalPosition, int>) point -> map.Change(point, increment)) Map.empty

    let rec calculateFuel steps =
        if steps > 0 then
            steps + calculateFuel (steps - 1)
        else
            0

    let calculateFuelToMove target position =
        calculateFuel (abs (target - position))

    let calculateFuelToMoveToPosition target (crabPositions: Map<HorizontalPosition, int>) =
        crabPositions
        |> Map.toSeq
        |> Seq.fold
            (fun totalFuel (position, crabs) ->
                totalFuel
                + (calculateFuelToMove target position) * crabs)
            0

    let calculateLeastFuelRequiredToAlign (crabPositions: Map<HorizontalPosition, int>) =
        [ crabPositions.Keys |> Seq.min .. crabPositions.Keys |> Seq.max ]
        |> Seq.map (fun target -> calculateFuelToMoveToPosition target crabPositions)
        |> Seq.min

    type Tests(output: ITestOutputHelper) =
        let testDataInput = "16,1,2,0,4,2,7,1,2,14"

        let testData =
            testDataInput
            |> parseInput
            |> countCrabsAtEachPosition

        [<Fact>]
        let ``Calculates fuel required to move a number of steps`` () = Assert.Equal(6, calculateFuel 3)

        [<Fact>]
        let ``Calculates fuel required to move from 16 to 5`` () = Assert.Equal(66, calculateFuelToMove 16 5)

        [<Fact>]
        let ``Calculates fuel required to move from 1 to 5`` () = Assert.Equal(10, calculateFuelToMove 1 5)

        [<Fact>]
        let ``Calculates fuel required to move all crabs to position 2`` () =
            Assert.Equal(206, (testData |> calculateFuelToMoveToPosition 2))

        [<Fact>]
        let ``Calculates fuel required to move all crabs to position 5`` () =
            Assert.Equal(168, (testData |> calculateFuelToMoveToPosition 5))

        [<Fact>]
        let ``Calculates least amount of fuel required to align all crabs`` () =
            Assert.Equal(168, (testData |> calculateLeastFuelRequiredToAlign))

        [<Fact>]
        let ``Calculates least amount of fuel required to align all crabs with real data`` () =
            let result =
                File.ReadAllText "data/day7input.txt"
                |> parseInput
                |> countCrabsAtEachPosition
                |> calculateLeastFuelRequiredToAlign

            output.WriteLine(result.ToString())
