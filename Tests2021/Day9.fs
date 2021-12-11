module Tests2021.Day9

open System
open System.IO
open Xunit
open Xunit.Abstractions

type Height = int

let parseHeightmapRow (input: string) : seq<Height> = input |> Seq.map Convert.ToInt32

let parseInput (input: seq<string>) : Height [,] =
    let heightmap: Height [,] =
        Array2D.init (input |> Seq.length) (input |> Seq.head |> Seq.length) (fun _ _ -> 0)

    input
    |> Seq.iteri
        (fun y line ->
            parseHeightmapRow line
            |> (Seq.iteri (fun x c -> Array2D.set heightmap y x ((int c) - int '0'))))

    heightmap

let getHeight (heightmap: Height [,]) y x : Height option =
    let length1: int = heightmap |> Array2D.length1
    let length2: int = heightmap |> Array2D.length2

    if x < 0 then None
    elif x > length2 - 1 then None
    elif y < 0 then None
    elif y > length1 - 1 then None
    else Some heightmap.[y, x]

let getNeighbouringHeights (heightmap: Height [,]) y x : Height seq =
    let neighbouringHeights =
        [ getHeight heightmap (y - 1) x
          getHeight heightmap (y + 1) x
          getHeight heightmap y (x - 1)
          getHeight heightmap y (x + 1) ]

    neighbouringHeights
    |> Seq.filter (fun h -> h.IsSome)
    |> Seq.map (fun h -> h.Value)

let isLowPoint (heightmap: Height [,]) y x =
    let height = heightmap.[y, x]

    getNeighbouringHeights heightmap y x
    |> Seq.toList
    |> Seq.forall (fun h -> h > height)

let findRowLowPoints (heightmap: Height [,]) y : Height seq =
    [ 0 .. (heightmap |> Array2D.length2) - 1 ]
    |> Seq.filter (isLowPoint heightmap y)
    |> Seq.map (fun x -> heightmap.[y, x])

let findLowPoints (heightmap: Height [,]) : Height seq =
    [ 0 .. (heightmap |> Array2D.length1) - 1 ]
    |> Seq.map (findRowLowPoints heightmap)
    |> Seq.concat

let sumRiskLevels (heightmap: Height [,]) : int =
    heightmap
    |> findLowPoints
    |> Seq.map (fun height -> height + 1)
    |> Seq.sum

type Tests(output: ITestOutputHelper) =

    let example =
        "2199943210
3987894921
9856789892
8767896789
9899965678"

    [<Fact>]
    let ``Finds low points`` () =
        Assert.Equal(
            [ 0; 1; 5; 5 ],
            example.Split Environment.NewLine
            |> parseInput
            |> findLowPoints
            |> Seq.sort
        )

    [<Fact>]
    let ``Sums risk levels`` () =
        Assert.Equal(
            15,
            example.Split Environment.NewLine
            |> parseInput
            |> sumRiskLevels
        )

    [<Fact>]
    let ``Sums risk levels with real data`` () =
        let result =
            File.ReadAllLines "data/day9input.txt"
            |> parseInput
            |> sumRiskLevels

        output.WriteLine(result.ToString())
