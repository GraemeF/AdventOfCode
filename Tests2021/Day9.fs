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

let isLocation (heightmap: Height [,]) y x : bool =
    let length1: int = heightmap |> Array2D.length1
    let length2: int = heightmap |> Array2D.length2

    if x < 0 then false
    elif x > length2 - 1 then false
    elif y < 0 then false
    elif y > length1 - 1 then false
    else true

let getHeight (heightmap: Height [,]) y x : Height option =
    if (isLocation heightmap y x) then
        None
    else
        Some heightmap.[y, x]

let getNeighbouringLocations (heightmap: Height [,]) (y: int) (x: int) : (int * int) seq =
    [ ((y - 1), x)
      ((y + 1), x)
      (y, (x - 1))
      (y, (x + 1)) ]
    |> Seq.filter (fun (y, x) -> isLocation heightmap y x)

let getNeighbouringHeights (heightmap: Height [,]) y x : Height seq =
    getNeighbouringLocations heightmap y x
    |> Seq.map (fun (y, x) -> heightmap.[y, x])

let isLowPoint (heightmap: Height [,]) y x =
    let height = heightmap.[y, x]

    getNeighbouringHeights heightmap y x
    |> Seq.toList
    |> Seq.forall (fun h -> h > height)

let findRowLowPointLocations (heightmap: Height [,]) y : (int * int) seq =
    [ 0 .. (heightmap |> Array2D.length2) - 1 ]
    |> Seq.filter (isLowPoint heightmap y)
    |> Seq.map (fun x -> (y, x))

let findLowPointLocations (heightmap: Height [,]) : (int * int) seq =
    [ 0 .. (heightmap |> Array2D.length1) - 1 ]
    |> Seq.map (findRowLowPointLocations heightmap)
    |> Seq.concat

let findLowPointHeights (heightmap: Height [,]) : Height seq =
    heightmap
    |> findLowPointLocations
    |> Seq.map (fun (y, x) -> heightmap.[y, x])

let sumRiskLevels (heightmap: Height [,]) : int =
    heightmap
    |> findLowPointHeights
    |> Seq.map (fun height -> height + 1)
    |> Seq.sum

let isInBasin (heightmap: Height [,]) (y, x) = heightmap.[y, x] < 9

let rec getBasinLocations (heightmap: Height [,]) (basinLocations: (int * int) list) (lowY, lowX) : (int * int) list =
    let newBasinLocations = (lowY, lowX) :: basinLocations

    getNeighbouringLocations heightmap lowY lowX
    |> Seq.except basinLocations
    |> Seq.filter (isInBasin heightmap)
    |> Seq.fold (getBasinLocations heightmap) newBasinLocations
    |> Seq.distinct
    |> Seq.toList

let rec measureBasinSize (heightmap: Height [,]) (basinLocations: (int * int) list) lowPointLocation : int =
    getBasinLocations heightmap [] lowPointLocation
    |> Seq.length

let findBasinSizes (heightmap: Height [,]) : int seq =
    heightmap
    |> findLowPointLocations
    |> Seq.map (measureBasinSize heightmap [])

let findLargestBasinSizes qty (heightmap: Height [,]) : int seq =
    heightmap
    |> findLowPointLocations
    |> Seq.map (measureBasinSize heightmap [])
    |> Seq.sortDescending
    |> Seq.take qty

let multiplyLargestBasinSizes qty (heightmap: Height [,]) : int =
    heightmap
    |> findLargestBasinSizes qty
    |> Seq.reduce (fun a b -> a * b)

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
            |> findLowPointHeights
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
    let ``Finds basin sizes`` () =
        Assert.Equal(
            [ 14; 9; 9 ],
            example.Split Environment.NewLine
            |> parseInput
            |> findLargestBasinSizes 3
        )

    [<Fact>]
    let ``Finds multiplied basin sizes`` () =
        Assert.Equal(
            1134,
            example.Split Environment.NewLine
            |> parseInput
            |> multiplyLargestBasinSizes 3
        )

    [<Fact>]
    let ``Sums risk levels with real data`` () =
        let result =
            File.ReadAllLines "data/day9input.txt"
            |> parseInput
            |> sumRiskLevels

        output.WriteLine(result.ToString())

    [<Fact>]
    let ``Multiplies largest basin sizes with real data`` () =
        let result =
            File.ReadAllLines "data/day9input.txt"
            |> parseInput
            |> multiplyLargestBasinSizes 3

        output.WriteLine(result.ToString())
