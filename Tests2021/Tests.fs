module Tests

open System
open System.IO
open Xunit
open Xunit.Abstractions

let IsDeeper a b = b > a

let rec Sweep (last: Option<int>) (rest: List<int>) : int =
    if rest.IsEmpty then
        0
    else
        let next = Some(rest.Head)

        (if last.IsSome && IsDeeper last next then
             1
         else
             0)
        + Sweep next rest.Tail

let SonarSweep depths : int = Sweep None depths

type Part1(output: ITestOutputHelper) =
    [<Fact>]
    let ``My test`` () =
        Assert.Equal(
            7,
            [ 199
              200
              208
              210
              200
              207
              240
              269
              260
              263 ]
            |> SonarSweep
        )

    [<Fact>]
    let ``for reals`` () =
        let result =
            File.ReadAllLines "data/day1input.txt"
            |> Array.toList
            |> List.map Int32.Parse
            |> SonarSweep

        output.WriteLine(result.ToString())
