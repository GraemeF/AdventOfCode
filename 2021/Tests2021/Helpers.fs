namespace Helpers

open System.Collections
open FluentAssertions

module Helpers =

    /// structurally compares two enumerables
    let public shouldBeEquivalentsTo expected (actual: 'a :> IEnumerable) =
        actual
            .Should()
            .BeEquivalentTo(expected :> IEnumerable, "")
        |> ignore
