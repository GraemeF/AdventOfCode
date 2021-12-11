namespace AdventOfCode

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open Xunit
open Xunit.Abstractions

module Day8 =

    type Entry =
        { patterns: string array
          output: string array }

    let easyPatternLengths = [| 2; 3; 4; 7 |]

    let parsePatterns (input: string) =
        input.Split ' '
        |> Seq.map (fun e -> e.Trim '|')
        |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace s))

    let parseEntry (input: string) : Entry =
        let parts = input.Split '|'
        let uniqueSignalPatterns = parts.[0] |> parsePatterns
        let outputValue = parts.[1] |> parsePatterns

        { patterns = uniqueSignalPatterns |> Seq.toArray
          output = outputValue |> Seq.toArray }

    let parseInput (input: string seq) : Entry array =
        input |> Seq.map parseEntry |> Seq.toArray

    let getEasyDigits (entries: Entry seq) =
        entries
        |> Seq.map (fun e -> e.output)
        |> Seq.concat
        |> Seq.filter (fun p -> easyPatternLengths |> Array.contains p.Length)

    let countEasyDigits (entries: Entry seq) = entries |> getEasyDigits |> Seq.length

    let digitSegments =
        dict [ (0, "abcefg")
               (1, "cf")
               (2, "acdeg")
               (3, "acdfg")
               (4, "bcdf")
               (5, "abdfg")
               (6, "abdefg")
               (7, "acf")
               (8, "abcdefg")
               (9, "abcdfg") ]

    let blankDisplay =
        ImmutableArray.Create<bool>([| 1 .. 7 |] |> Array.map (fun _ -> false))

    let stringToBitmap: Char seq -> ImmutableArray<bool> =
        Seq.fold (fun result c -> result.SetItem(((int c) - (int 'a')), true)) blankDisplay

    let toString (c: char) (s: char seq) : string = c :: (s |> Seq.toList) |> String.Concat

    let rec findWiringCombinations (remainingSegments: char seq) : string seq =
        remainingSegments
        |> Seq.map
            (fun (c: char) ->
                let result =
                    (remainingSegments
                     |> (Seq.except [ c ])
                     |> findWiringCombinations
                     |> Seq.map (toString c))

                if result |> Seq.isEmpty then
                    [ $"{c}" ] |> List.toSeq
                else
                    result)
        |> Seq.concat

    let unshuffledDisplayMappings = "abcdefg"
    let determineDisplayMappings patterns = "deafgbc"

    let sortedPattern (s: char seq) : string = s |> Seq.sort |> String.Concat

    let isMatch (a: char seq) (b: char seq) : bool =
        (a |> sortedPattern) = (b |> sortedPattern)

    let determineDigitMappings displayMappings =
        digitSegments
        |> Seq.map (fun mapping -> mapping) // TODO
        |> Seq.fold (fun digitMappings mapping -> digitMappings |> Map.add mapping.Key mapping.Value) Map.empty

    let matchDigit (mappings: IDictionary<int, string>) (pattern: string) : int =
        mappings
        |> Seq.find (fun x -> isMatch pattern x.Value)
        |> fun x -> x.Key


    let matchDigits output mappings : int seq = output |> Seq.map (matchDigit mappings)

    let getOutputDigits (entry: Entry) : int seq =
        entry.patterns
        |> determineDisplayMappings
        |> determineDigitMappings
        |> matchDigits entry.output

    type Tests(output: ITestOutputHelper) =

        let example =
            "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

        let largerExample =
            "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

        [<Fact>]
        let ``Parses input`` () =
            output.WriteLine(
                sprintf
                    "%A"
                    (largerExample.Split Environment.NewLine
                     |> parseInput
                     |> Seq.toArray)
            )

        [<Fact>]
        let ``Gets easy digits`` () =
            output.WriteLine(
                largerExample.Split Environment.NewLine
                |> parseInput
                |> getEasyDigits
                |> String.concat ","
            )

        [<Fact>]
        let ``Counts number of times easy digits occur`` () =
            Assert.Equal(
                26,
                largerExample.Split Environment.NewLine
                |> parseInput
                |> countEasyDigits
            )

        [<Fact>]
        let ``Converts a string to a bitmap`` () =
            let expected =
                ImmutableArray.Create(true, false, true, false, false, true, false)

            let actual = "acf" |> stringToBitmap
            Helpers.Helpers.shouldBeEquivalentsTo expected actual

        [<Fact>]
        let ``Finds wiring combinations`` () =
            Assert.Equal(6, "obc" |> findWiringCombinations |> Seq.length)

        [<Fact>]
        let ``Determines display mappings`` () =
            let mappings =
                example |> parseEntry |> determineDisplayMappings

            Assert.Equal("deafgbc", mappings)

        [<Fact>]
        let ``Determines digit mappings`` () =
            let mappings =
                example |> parseEntry |> determineDigitMappings

            Assert.True(isMatch "gcdfa" mappings.[2], mappings.[2])

        [<Fact>]
        let ``Determines output digits`` () =
            Assert.Equal([ 5; 3; 5; 3 ], example |> parseEntry |> getOutputDigits)

        [<Fact>]
        let ``Counts number of times easy digits occur with real data`` () =
            let result =
                File.ReadAllLines "data/day8input.txt"
                |> parseInput
                |> countEasyDigits

            output.WriteLine(result.ToString())
