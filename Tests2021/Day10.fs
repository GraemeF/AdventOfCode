module Tests2021.Day10

open System
open System.Collections.Immutable
open System.IO
open FsUnit
open Xunit
open Xunit.Abstractions

type Pair =
    { opener: char
      closer: char
      score: int }

let legalPairs: Pair list =
    [ { opener = '('
        closer = ')'
        score = 3 }
      { opener = '['
        closer = ']'
        score = 57 }
      { opener = '{'
        closer = '}'
        score = 1197 }
      { opener = '<'
        closer = '>'
        score = 25137 } ]

let findOpener c =
    legalPairs |> Seq.tryFind (fun p -> p.opener = c)

let findCloser c =
    legalPairs |> Seq.tryFind (fun p -> p.closer = c)


let processCharacter (openChunks: ImmutableStack<Pair>, illegalCharacter: char option) (character: char) =
    if illegalCharacter.IsSome then
        (openChunks, illegalCharacter)
    else
        let pair = findOpener character

        if pair.IsSome then
            (openChunks.Push(pair.Value), None)
        else
            let topChunk = openChunks.Peek()

            if topChunk.closer = character then
                (openChunks.Pop(), None)
            else
                (openChunks, Some character)

let findFirstIllegalCharacter line : char option =
    line
    |> Seq.fold processCharacter (ImmutableStack<Pair>.Empty, None)
    |> snd

let totalScores (lines: string seq) : int =
    lines
    |> Seq.map findFirstIllegalCharacter
    |> Seq.filter (fun x -> x.IsSome)
    |> Seq.sumBy (fun x -> (findCloser x.Value).Value.score)

type Tests(output: ITestOutputHelper) =

    let exampleValidChunks =
        [ "()"
          "[]"
          "([])"
          "{()()()}"
          "<([{}])>"
          "[<>({}){}[([])<>]]"
          "(((((((((())))))))))" ]

    let exampleCorruptedChunks =
        [ "(]"
          "{()()()>"
          "(((()))}"
          "<([]){()}[{}])" ]

    let example =
        "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

    [<Fact>]
    let ``Recognises valid chunks`` () =
        exampleValidChunks
        |> Seq.map findFirstIllegalCharacter
        |> Seq.toList
        |> List.map (fun x -> x.IsNone)
        |> should equal (List.init exampleValidChunks.Length (fun _ -> true))

    [<Fact>]
    let ``Recognises corrupted lines`` () =
        exampleCorruptedChunks
        |> Seq.map findFirstIllegalCharacter
        |> Seq.toList
        |> List.map (fun (x: char option) -> x.IsSome)
        |> should equal (List.init exampleCorruptedChunks.Length (fun _ -> true))

    [<Fact>]
    let ``Totals scores`` () =
        example.Split Environment.NewLine
        |> totalScores
        |> should equal 26397

    [<Fact>]
    let ``Totals scores with real data`` () =
        File.ReadAllLines "data/day10input.txt"
        |> totalScores
        |> fun x -> output.WriteLine(x.ToString())
