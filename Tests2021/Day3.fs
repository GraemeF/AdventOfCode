namespace AdventOfCode

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

    let getBitValue index shift diagnosticReport =
        (getDominantBits index diagnosticReport) <<< shift

    let findGammaRate (diagnosticReport: seq<string>) =
        let length = (diagnosticReport |> Seq.head).Length

        [ 0 .. length - 1 ]
        |> Seq.fold
            (fun total index ->
                total
                + (getBitValue index (length - index - 1) diagnosticReport))
            0

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
        let ``Finds gamma rate`` () =
            Assert.Equal(22, testData.Split '\n' |> findGammaRate)
