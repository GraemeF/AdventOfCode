namespace AdventOfCode

open System
open System.IO
open Xunit
open Xunit.Abstractions

module Day6 =

    type Lanternfish = int
    type School = seq<Lanternfish>

    let simulateLanternfish (lanternfish: Lanternfish) : Lanternfish * Option<Lanternfish> =
        match lanternfish with
        | 0 -> (6, Some(8))
        | _ -> (lanternfish - 1, None)

    let rec simulateSchool (school: Lanternfish seq) : Lanternfish list =
        school
        |> Seq.fold
            (fun (oldFish: Lanternfish list, newFish: Lanternfish list) lanternfish ->
                let fish, spawn = simulateLanternfish lanternfish

                (fish :: oldFish,
                 if spawn.IsSome then
                     spawn.Value :: newFish
                 else
                     newFish))
            ([], [])
        |> fun (oldFish, newFish) -> List.append (oldFish |> List.rev) newFish

    let rec simulate days (school: School) =
        if days > 0 then
            school |> simulateSchool |> simulate (days - 1)
        else
            school

    let countFish (school: School) = school |> Seq.length

    let parseInput (input: string) =
        input.Split ',' |> Seq.map Convert.ToInt32

    type Tests(output: ITestOutputHelper) =
        let testDataInput = "3,4,3,1,2"

        let testData = testDataInput |> parseInput

        [<Fact>]
        let ``Simulates a lanternfish`` () = Assert.Equal([ 2 ], simulate 1 [ 3 ])

        [<Fact>]
        let ``Simulates a lanternfish`` () = Assert.Equal([ 1 ], simulate 2 [ 3 ])

        [<Fact>]
        let ``Simulates a lanternfish`` () = Assert.Equal([ 0 ], simulate 3 [ 3 ])

        [<Fact>]
        let ``Simulates a lanternfish`` () =
            Assert.Equal([ 6; 8 ], simulate 4 [ 3 ])

        [<Fact>]
        let ``Simulates a lanternfish`` () =
            Assert.Equal([ 5; 7 ], simulate 5 [ 3 ])

        [<Fact>]
        let ``Simulates a school of lanternfish`` () =
            Assert.Equal(
                [ 6
                  0
                  6
                  4
                  5
                  6
                  0
                  1
                  1
                  2
                  6
                  0
                  1
                  1
                  1
                  2
                  2
                  3
                  3
                  4
                  6
                  7
                  8
                  8
                  8
                  8 ],
                testData |> simulate 18
            )

        [<Fact>]
        let ``Counts lanternfish`` () =
            Assert.Equal(26, testData |> simulate 18 |> countFish)

        [<Fact>]
        let ``Simulates lanternfish with real data`` () =
            let result =
                File.ReadAllText "data/day6input.txt"
                |> parseInput
                |> simulate 80
                |> countFish

            output.WriteLine(result.ToString())
