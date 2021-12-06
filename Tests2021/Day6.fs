namespace AdventOfCode

open System
open System.IO
open Xunit
open Xunit.Abstractions

module Day6 =

    type Lanternfish = int
    type School = uint64 array

    let createSchool: School = Array.create 9 0UL

    let simulateLanternfish (lanternfish: Lanternfish) : Lanternfish list =
        match lanternfish with
        | 0 -> [ 6; 8 ]
        | _ -> [ lanternfish - 1 ]

    let simulateSchool (school: School) : School =
        let newSchool = createSchool

        for timer in [ 0 .. 8 ] do
            let qty = school.[timer]
            Array.set newSchool timer qty

        newSchool


    let rec simulate days (school: School) =
        if days > 0 then
            school |> simulateSchool |> simulate (days - 1)
        else
            school

    let countFish (school: School) = school |> Seq.sum

    let parseInput (input: string) =
        let school = createSchool

        input.Split ','
        |> Seq.map Convert.ToInt32
        |> Seq.iter (fun (fish: Lanternfish) -> Array.set school fish (school.[fish] + 1UL))

        school

    type Tests(output: ITestOutputHelper) =
        let testDataInput = "3,4,3,1,2"

        let testData = testDataInput |> parseInput

        let SchoolsEqual (a: int array) (b: School) =
            Assert.Equal(a |> Seq.map Convert.ToUInt64 |> Seq.toArray, b |> Seq.toList)

        [<Fact>]
        let ``Simulates a lanternfish`` () =
            SchoolsEqual [| 0
                            0
                            1
                            0
                            0
                            0
                            0
                            0
                            0 |],
            simulate
                1
                [| 0UL
                   0UL
                   0UL
                   1UL
                   0UL
                   0UL
                   0UL
                   0UL
                   0UL |]

        [<Fact>]
        let ``Simulates a lanternfish`` () =
            SchoolsEqual [| 0
                            1
                            0
                            0
                            0
                            0
                            0
                            0
                            0 |],
            simulate
                2
                [| 0UL
                   0UL
                   0UL
                   1UL
                   0UL
                   0UL
                   0UL
                   0UL
                   0UL |]

        [<Fact>]
        let ``Simulates a lanternfish`` () =
            SchoolsEqual [| 1
                            0
                            0
                            0
                            0
                            0
                            0
                            0
                            0 |],
            simulate
                3
                [| 0UL
                   0UL
                   0UL
                   1UL
                   0UL
                   0UL
                   0UL
                   0UL
                   0UL |]

        [<Fact>]
        let ``Simulates a lanternfish`` () =
            SchoolsEqual [| 0
                            0
                            0
                            0
                            0
                            0
                            1
                            0
                            1 |],
            simulate
                4
                [| 0UL
                   0UL
                   0UL
                   1UL
                   0UL
                   0UL
                   0UL
                   0UL
                   0UL |]

        [<Fact>]
        let ``Simulates a lanternfish`` () =
            SchoolsEqual [| 0
                            0
                            0
                            0
                            0
                            1
                            0
                            1
                            0 |],
            simulate
                5
                [| 0UL
                   0UL
                   0UL
                   1UL
                   0UL
                   0UL
                   0UL
                   0UL
                   0UL |]

        [<Fact>]
        let ``Counts lanternfish`` () =
            Assert.Equal(26UL, testData |> simulate 18 |> countFish)

        [<Fact>]
        let ``Counts lanternfish`` () =
            Assert.Equal(26984457539UL, testData |> simulate 256 |> countFish)

        [<Fact>]
        let ``Simulates lanternfish with real data`` () =
            let result =
                File.ReadAllText "data/day6input.txt"
                |> parseInput
                |> simulate 80
                |> countFish

            output.WriteLine(result.ToString())
