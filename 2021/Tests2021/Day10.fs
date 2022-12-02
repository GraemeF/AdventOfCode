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
      syntaxErrorScore: UInt64
      autocompleteScore: UInt64 }

let legalPairs: Pair list =
    [ { opener = '('
        closer = ')'
        syntaxErrorScore = 3UL
        autocompleteScore = 1UL }
      { opener = '['
        closer = ']'
        syntaxErrorScore = 57UL
        autocompleteScore = 2UL }
      { opener = '{'
        closer = '}'
        syntaxErrorScore = 1197UL
        autocompleteScore = 3UL }
      { opener = '<'
        closer = '>'
        syntaxErrorScore = 25137UL
        autocompleteScore = 4UL } ]

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

let processLine line =
    line
    |> Seq.fold processCharacter (ImmutableStack<Pair>.Empty, None)

let findFirstIllegalCharacter line : char option = line |> processLine |> snd

let totalSyntaxErrorScores (lines: string seq) : UInt64 =
    lines
    |> Seq.map findFirstIllegalCharacter
    |> Seq.filter (fun x -> x.IsSome)
    |> Seq.sumBy (fun x -> (findCloser x.Value).Value.syntaxErrorScore)

let findIncompleteLineOpenChunks (lines: string seq) : ImmutableStack<Pair> seq =
    lines
    |> Seq.map processLine
    |> Seq.filter (fun x -> (snd x).IsNone)
    |> Seq.map fst
    |> Seq.filter (fun x -> not (Seq.isEmpty x))

let findAutocompleteScore =
    Seq.fold (fun total openChunk -> (total * 5UL) + openChunk.autocompleteScore) 0UL

let findAutocompleteScores (lines: string seq) : UInt64 seq =
    lines
    |> findIncompleteLineOpenChunks
    |> Seq.map findAutocompleteScore

let findMiddleAutocompleteScore (lines: string seq) : UInt64 =
    let scores =
        lines
        |> findAutocompleteScores
        |> Seq.sort
        |> Seq.toList

    ((scores |> Seq.length) % 2) |> should equal 1

    scores.[(scores |> Seq.length) / 2]

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
    let ``Totals syntax error scores`` () =
        example.Split Environment.NewLine
        |> totalSyntaxErrorScores
        |> should equal 26397UL

    [<Fact>]
    let ``Finds autocomplete scores`` () =
        example.Split Environment.NewLine
        |> findAutocompleteScores
        |> Seq.toList
        |> should
            equal
            [ 288957UL
              5566UL
              1480781UL
              995444UL
              294UL ]

    [<Fact>]
    let ``Find middle autocomplete score`` () =
        example.Split Environment.NewLine
        |> findMiddleAutocompleteScore
        |> should equal 288957UL

    [<Fact>]
    let ``Totals syntax error scores with real data`` () =
        File.ReadAllLines "data/day10input.txt"
        |> totalSyntaxErrorScores
        |> fun x -> output.WriteLine(x.ToString())

    [<Fact>]
    let ``Finds autocomplete scores with real data`` () =
        File.ReadAllLines "data/day10input.txt"
        |> findAutocompleteScores
        |> Seq.map (fun x -> x.ToString())
        |> (fun x -> String.Join(", ", x))
        |> fun x -> output.WriteLine(x.ToString())

    [<Fact>]
    let ``Finds middle autocomplete score with real data`` () =
        let score =
            File.ReadAllLines "data/day10input.txt"
            |> Seq.map (fun s -> s.Trim())
            |> findMiddleAutocompleteScore

        score |> should be (greaterThan 228508708UL)
        score |> fun x -> output.WriteLine(x.ToString())

// 228508708 is too low
