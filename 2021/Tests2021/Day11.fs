﻿module Tests2021.Day11

open System
open System.IO
open FsUnit
open Xunit
open Xunit.Abstractions

type EnergyLevel = byte

let parseEnergyLevelMapRow (input: string) = input |> Seq.map Convert.ToByte

let parseInput (input: seq<string>) : EnergyLevel [,] =
    let energyLevelMap: EnergyLevel [,] =
        Array2D.init (input |> Seq.length) (input |> Seq.head |> Seq.length) (fun _ _ -> 0uy)

    input
    |> Seq.iteri
        (fun y line ->
            parseEnergyLevelMapRow line
            |> (Seq.iteri (fun x c -> Array2D.set energyLevelMap y x ((byte c) - byte '0'))))

    energyLevelMap

let isLocation (energyLevelMap: EnergyLevel [,]) y x : bool =
    let length1: int = energyLevelMap |> Array2D.length1
    let length2: int = energyLevelMap |> Array2D.length2

    if x < 0 then false
    elif x > length2 - 1 then false
    elif y < 0 then false
    elif y > length1 - 1 then false
    else true

let getEnergyLevel (energyLevelMap: EnergyLevel [,]) y x : EnergyLevel option =
    if (isLocation energyLevelMap y x) then
        Some energyLevelMap.[y, x]
    else
        None

let setEnergyLevel (energyLevelMap: EnergyLevel [,]) (y: int) (x: int) (energyLevel: EnergyLevel) : EnergyLevel [,] =
    energyLevelMap
    |> fun a -> Array2D.set a y x energyLevel

    energyLevelMap

let getNeighbouringLocations (energyLevelMap: EnergyLevel [,]) (y: int) (x: int) : (int * int) seq =
    [ ((y + 1), (x + 1))
      ((y + 1), (x - 1))
      ((y + 1), x)
      ((y - 1), (x + 1))
      ((y - 1), (x - 1))
      ((y - 1), x)
      (y, (x + 1))
      (y, (x - 1)) ]
    |> Seq.filter (fun (y, x) -> isLocation energyLevelMap y x)

let getNeighbouringEnergyLevels (energyLevelMap: EnergyLevel [,]) y x : EnergyLevel seq =
    getNeighbouringLocations energyLevelMap y x
    |> Seq.map (fun (y, x) -> energyLevelMap.[y, x])

let getRowLocations (energyLevelMap: EnergyLevel [,]) y : (int * int) seq =
    [ 0 .. (energyLevelMap |> Array2D.length2) - 1 ]
    |> Seq.map (fun x -> (y, x))

let getAllLocations (energyLevelMap: EnergyLevel [,]) : (int * int) seq =
    [ 0 .. (energyLevelMap |> Array2D.length1) - 1 ]
    |> Seq.map (getRowLocations energyLevelMap)
    |> Seq.concat

let rec increase (map: EnergyLevel [,]) ((y, x): int * int) : EnergyLevel [,] =
    let initialEnergyLevel = getEnergyLevel map y x

    if initialEnergyLevel.IsNone then
        map
    else
        let newEnergyLevel = initialEnergyLevel.Value + 1uy
        let increasedMap = setEnergyLevel map y x newEnergyLevel

        if newEnergyLevel = 10uy then
            (getNeighbouringLocations map y x)
            |> Seq.fold increase increasedMap
        else
            increasedMap

let reset (map: EnergyLevel [,]) ((y, x): int * int) : EnergyLevel [,] * bool =
    let energyLevel = getEnergyLevel map y x

    if energyLevel.IsNone then
        (map, false)
    else if energyLevel.Value > 9uy then
        ((setEnergyLevel map y x 0uy), true)
    else
        (map, false)

let step (map: EnergyLevel [,]) : uint * EnergyLevel [,] =
    let increasedMap =
        map |> getAllLocations |> Seq.fold increase map

    increasedMap
    |> getAllLocations
    |> Seq.fold
        (fun (flashCount, updatedMap) location ->
            let flashedMap, didFlash = reset updatedMap location
            ((flashCount + if didFlash then 1u else 0u), flashedMap))
        (0u, increasedMap)

let countFlashes steps map =
    [ 1 .. steps ]
    |> Seq.fold
        (fun (totalFlashes, map) _ ->
            let newFlashes, newMap = step map
            (newFlashes + totalFlashes, newMap))
        (0u, map)
    |> fst

let findFirstStepWhereAllFlash map =
    Seq.initInfinite (fun x -> x + 2)
    |> Seq.takeWhile (fun stepNumber -> (step map) |> fst < uint (map.GetLength(0) * map.GetLength(1)))
    |> Seq.last

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
        |> parseInput
        |> countFlashes 100
        |> should equal 1656u


    [<Fact>]
    let ``Finds synchronised flash`` () =
        example.Split Environment.NewLine
        |> parseInput
        |> findFirstStepWhereAllFlash
        |> should equal 195

    [<Fact>]
    let ``Counts flashes with real data`` () =
        let flashes =
            File.ReadAllLines "data/day11input.txt"
            |> parseInput
            |> countFlashes 100

        flashes |> fun x -> output.WriteLine(x.ToString())

    [<Fact>]
    let ``Finds synchronised flash with real data`` () =
        let stepNumber =
            File.ReadAllLines "data/day11input.txt"
            |> parseInput
            |> findFirstStepWhereAllFlash

        stepNumber
        |> fun x -> output.WriteLine(x.ToString())
