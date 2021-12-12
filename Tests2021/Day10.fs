module Tests2021.Day10

open FsUnit
open Xunit
open Xunit.Abstractions

let findFirstIllegalCharacter line : char option = None

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
        exampleValidChunks
        |> Seq.map findFirstIllegalCharacter
        |> Seq.toList
        |> List.map (fun (x: char option) -> x.IsSome)
        |> should equal (List.init exampleValidChunks.Length (fun _ -> true))
