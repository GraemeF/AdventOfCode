namespace AdventOfCode

open System.IO
open Xunit
open Xunit.Abstractions

module Day3 =

    let getBit (index: int) (line: string) : int =
        match line.[index] with
        | '0' -> 0
        | '1' -> 1
        | _ -> failwith "wtf is this"

    let parseBinaryNumber (input: string) : int =
        [ 0 .. input.Length - 1 ]
        |> Seq.sumBy
            (fun index ->
                (getBit index input)
                <<< (input.Length - index - 1))

    let countBits index report =
        Seq.map (getBit index) report
        |> Seq.fold
            (fun (zeros, ones) bit ->
                if bit = 1 then
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
        let length, indexes = diagnosticReport |> getBitIndexes

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

    let getBitToMatch bitCriteria index candidates : int =
        countBits index candidates |> bitCriteria

    let filterCandidates (candidates: seq<string>) (bitCriteria: int * int -> int) (index: int) : seq<string> =
        if candidates |> Seq.length = 1 then
            candidates
        else
            let bitToMatch =
                getBitToMatch bitCriteria index candidates

            candidates
            |> Seq.filter (fun candidate -> getBit index candidate = bitToMatch)

    let calculateRating (bitCriteria: int * int -> int) diagnosticReport =
        let _, indexes = diagnosticReport |> getBitIndexes

        indexes
        |> Seq.fold (fun candidates -> filterCandidates candidates bitCriteria) diagnosticReport
        |> Seq.head
        |> parseBinaryNumber

    let chooseOxygenGeneratorRatingBit ((zeros, ones): int * int) : int = if zeros > ones then 0 else 1
    let chooseCO2ScrubberRatingBit ((zeros, ones): int * int) : int = if ones < zeros then 1 else 0

    let calculateOxygenGeneratorRating diagnosticReport =
        calculateRating chooseOxygenGeneratorRatingBit diagnosticReport

    let calculateCO2ScrubberRating diagnosticReport =
        calculateRating chooseCO2ScrubberRatingBit diagnosticReport

    let calculateLifeSupportRating diagnosticReport =
        (diagnosticReport |> calculateOxygenGeneratorRating)
        * (diagnosticReport |> calculateCO2ScrubberRating)

    type Tests(output: ITestOutputHelper) =

        let testDataInput =
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

        let testData = testDataInput.Split '\n'

        [<Fact>]
        let ``Calculates gamma rate`` () =
            Assert.Equal(22, testData |> calculateGammaRate)

        [<Fact>]
        let ``Calculates epsilon rate`` () =
            Assert.Equal(9, testData |> calculateEpsilonRate)

        [<Fact>]
        let ``Calculates power consumption`` () =
            Assert.Equal(198, testData |> calculatePowerConsumption)

        [<Fact>]
        let ``Chooses oxygen generator rating bit value`` () =
            Assert.Equal(1, (5, 7) |> chooseOxygenGeneratorRatingBit)

        [<Fact>]
        let ``Chooses oxygen generator rating bit value`` () =
            Assert.Equal(0, (7, 5) |> chooseOxygenGeneratorRatingBit)

        [<Fact>]
        let ``Chooses oxygen generator rating bit value`` () =
            Assert.Equal(1, (1, 1) |> chooseOxygenGeneratorRatingBit)

        [<Fact>]
        let ``Chooses CO2 scrubber rating bit value`` () =
            Assert.Equal(0, (5, 7) |> chooseCO2ScrubberRatingBit)

        [<Fact>]
        let ``Chooses CO2 scrubber rating bit value`` () =
            Assert.Equal(1, (7, 5) |> chooseCO2ScrubberRatingBit)

        [<Fact>]
        let ``Chooses CO2 scrubber rating bit value`` () =
            Assert.Equal(0, (1, 1) |> chooseCO2ScrubberRatingBit)

        [<Fact>]
        let ``Finds oxygen generator rating bit value in test data`` () =
            Assert.Equal(
                1,
                testData
                |> countBits 0
                |> chooseOxygenGeneratorRatingBit
            )

        [<Fact>]
        let ``Counts bits`` () =
            Assert.Equal((5, 7), testData |> countBits 0)

        [<Fact>]
        let ``Finds CO2 scrubber rating bit`` () =
            Assert.Equal(
                0,
                testData
                |> getBitToMatch chooseCO2ScrubberRatingBit 0
            )

        [<Fact>]
        let ``Filters oxygen generator rating candidates`` () =
            Assert.Equal(
                [ "11110"
                  "10110"
                  "10111"
                  "10101"
                  "11100"
                  "10000"
                  "11001" ],
                testData
                |> fun candidates -> filterCandidates candidates chooseOxygenGeneratorRatingBit 0
            )

        [<Fact>]
        let ``Filters CO2 scrubber rating candidates`` () =
            Assert.Equal(
                [ "00100"
                  "01111"
                  "00111"
                  "00010"
                  "01010" ],
                testData
                |> fun candidates -> filterCandidates candidates chooseCO2ScrubberRatingBit 0
            )

        [<Fact>]
        let ``Calculates oxygen generator rating`` () =
            Assert.Equal(23, testData |> calculateOxygenGeneratorRating)

        [<Fact>]
        let ``Calculates CO2 scrubber rating`` () =
            Assert.Equal(10, testData |> calculateCO2ScrubberRating)

        [<Fact>]
        let ``Calculates life support rating`` () =
            Assert.Equal(230, testData |> calculateLifeSupportRating)

        [<Fact>]
        let ``Calculates power consumption with real data`` () =
            let result =
                File.ReadAllLines "data/day3input.txt"
                |> calculatePowerConsumption

            output.WriteLine(result.ToString())

        [<Fact>]
        let ``Calculates life support rating with real data`` () =
            let result =
                File.ReadAllLines "data/day3input.txt"
                |> calculateLifeSupportRating

            output.WriteLine(result.ToString())
