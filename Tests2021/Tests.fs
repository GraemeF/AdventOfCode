module Tests

open Xunit

let IsDown a b = b < a

let rec Sweep (last: Option<int>) (rest: List<int>) : int =
    if rest.IsEmpty then
        0
    else
        let next = Some(rest.Head)

        (if IsDown last next then 1 else 0)
        + Sweep next rest.Tail

let SonarSweep depths : int = Sweep None depths

type Tests() =
    [<Fact>]
    let ``My test`` () =
        Assert.Equal(
            2,
            [ 199
              200
              208
              210
              200
              207
              240
              269
              260
              263 ]
            |> SonarSweep
        )
