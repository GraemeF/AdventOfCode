namespace AdventOfCode

open System
open Xunit
open Xunit.Abstractions

module Day2 =

    type Direction =
        | down = 0
        | forward = 1
        | up = 2

    type Step = { direction: Direction; distance: int }
    type Position = { horizontal: int; depth: int }
    let start: Position = { Position.horizontal = 0; depth = 0 }

    let parseStep (step: string) =
        let parts = step.Split ' '
        let direction = parts.[0] |> Direction.Parse
        let distance = parts.[1] |> Int32.Parse

        { Step.direction = direction
          distance = distance }

    let parseCourse (course: string) = course.Split '\n' |> Seq.map parseStep

    let move (position: Position) (step: Step) =
        match step with
        | { Step.direction = Direction.up } ->
            { position with
                  depth = position.depth - step.distance }
        | { Step.direction = Direction.down } ->
            { position with
                  depth = position.depth + step.distance }
        | { Step.direction = Direction.forward } ->
            { position with
                  horizontal = position.horizontal + step.distance }
        | _ -> ArgumentOutOfRangeException() |> raise

    let calculate course =
        let finalPosition =
            course |> parseCourse |> Seq.fold move start

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
        let ``multiplies horizontal position by depth`` () = Assert.Equal(150, calculate testData)
