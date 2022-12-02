namespace AdventOfCode

open System
open System.IO
open Xunit
open Xunit.Abstractions

module Day2 =

    type Direction =
        | down = 0
        | forward = 1
        | up = 2

    type Step = { direction: Direction; distance: int }

    type Position =
        { horizontal: int
          depth: int
          aim: int }

    let start: Position =
        { Position.horizontal = 0
          depth = 0
          aim = 0 }

    let parseStep (step: string) =
        let parts = step.Split ' '
        let direction = parts.[0] |> Direction.Parse
        let distance = parts.[1] |> Int32.Parse

        { Step.direction = direction
          distance = distance }

    let parseCourse course = course |> Seq.map parseStep

    let move (position: Position) (step: Step) =
        match step with
        | { Step.direction = Direction.up } ->
            { position with
                  aim = position.aim - step.distance }
        | { Step.direction = Direction.down } ->
            { position with
                  aim = position.aim + step.distance }
        | { Step.direction = Direction.forward } ->
            { position with
                  horizontal = position.horizontal + step.distance
                  depth = position.depth + position.aim * step.distance }
        | _ -> ArgumentOutOfRangeException() |> raise

    let calculate (course: seq<Step>) : int =
        let finalPosition = course |> Seq.fold move start

        finalPosition.horizontal * finalPosition.depth

    type Tests(output: ITestOutputHelper) =
        let testData =
            "forward 5
down 5
forward 8
up 3
down 8
forward 2"

        [<Fact>]
        let ``does the thing with aim`` () =
            Assert.Equal(900, calculate (testData.Split '\n' |> parseCourse))

        [<Fact>]
        let ``for reals`` () =
            let result =
                File.ReadAllLines "data/day2input.txt"
                |> parseCourse
                |> calculate

            output.WriteLine(result.ToString())
