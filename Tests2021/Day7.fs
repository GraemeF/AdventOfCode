namespace AdventOfCode

open System
open Xunit.Abstractions

module Day7 =

    let parseInput (input: string) =
        input.Split ',' |> Seq.map Convert.ToInt32

    type Tests(output: ITestOutputHelper) =
        let testDataInput = "16,1,2,0,4,2,7,1,2,14"

        let testData = testDataInput |> parseInput
