namespace AdventOfCode

open System.IO
open Xunit
open Xunit.Abstractions

module Day3 =

    let getBit (index: int) (line: string) =
        match line.[index] with
        | '0' -> false
        | '1' -> true
        | _ -> failwith "wtf is this"

    let countBits index report =
        Seq.map (getBit index) report
        |> Seq.fold
            (fun (zeros, ones) bit ->
                if bit then
                    (zeros, ones + 1)
                else
                    (zeros + 1, ones))
            (0, 0)

    let getDominantBits index diagnosticReport =
        let zeros, ones = diagnosticReport |> countBits index
        if zeros > ones then 0 else 1

    let getGammaRateBitValue index shift diagnosticReport =
        (getDominantBits index diagnosticReport) <<< shift

    let getEpsilonRateBitValue index shift diagnosticReport =
        (1 - (getDominantBits index diagnosticReport))
        <<< shift

    let getBitIndexes (diagnosticReport: seq<string>) =
        let length = (diagnosticReport |> Seq.head).Length
        (length, [ 0 .. length - 1 ])
        
    let calculateRate rateFunction (diagnosticReport: seq<string>) =
        let length, indexes =diagnosticReport |> getBitIndexes
        indexes
        |> Seq.fold
            (fun total index ->
                total
                + (rateFunction index (length - index - 1) diagnosticReport))
            0

    let calculateGammaRate diagnosticReport =
        calculateRate getGammaRateBitValue diagnosticReport

    let calculateEpsilonRate diagnosticReport =
        calculateRate getEpsilonRateBitValue diagnosticReport

    let calculatePowerConsumption (diagnosticReport: seq<string>) =
        let gammaRate = calculateGammaRate diagnosticReport
        let epsilonRate = calculateEpsilonRate diagnosticReport

        gammaRate * epsilonRate

    let filterCandidates candidates index =
        candidates |> Seq.fold (fun candidates filterCandidates bitCriteria)
        
    let calculateRating bitCriteria diagnosticReport =
        let length, indexes =diagnosticReport |> getBitIndexes
        indexes |>
            Seq.fold (filterCandidates diagnosticReport)
    
    let calculateOxygenScrubberRating diagnosticReport =
        calculateRating (fun (zeros, ones) -> zeros > ones) diagnosticReport

    type Tests(output: ITestOutputHelper) =

        let testData =
            "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"

        [<Fact>]
        let ``Calculates gamma rate`` () =
            Assert.Equal(22, testData.Split '\n' |> calculateGammaRate)

        [<Fact>]
        let ``Calculates epsilon rate`` () =
            Assert.Equal(9, testData.Split '\n' |> calculateEpsilonRate)

        [<Fact>]
        let ``Calculates power consumption`` () =
            Assert.Equal(198, testData.Split '\n' |> calculatePowerConsumption)

        [<Fact>]
        let ``Calculates oxygen generator rating`` () =
            Assert.Equal(23, testData.Split '\n' |> calculateOxygenScrubberRating)

        [<Fact>]
        let ``Calculates power consumption with real data`` () =
            let result =
                File.ReadAllLines "data/day3input.txt"
                |> calculatePowerConsumption

            output.WriteLine(result.ToString())
