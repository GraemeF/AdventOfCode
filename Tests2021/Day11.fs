module Tests2021.Day11

open System
open FsUnit
open Xunit
open Xunit.Abstractions

let countFlashes _ = 0

type Tests(output: ITestOutputHelper) =

    let example =
        "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

    [<Fact>]
    let ``Counts flashes`` () =
        example.Split Environment.NewLine
        |> countFlashes
        |> should equal 1656